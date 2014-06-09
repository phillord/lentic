;;; linked-buffer-block.el --- Comment blocks in one buffer -*- lexical-binding: t -*-

;; This file is not part of Emacs

;; Author: Phillip Lord <phillip.lord@newcastle.ac.uk>
;; Maintainer: Phillip Lord <phillip.lord@newcastle.ac.uk>

;; The contents of this file are subject to the LGPL License, Version 3.0.
;;
;; Copyright (C) 2014, Phillip Lord, Newcastle University
;;
;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License
;; for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with this program. If not, see http://www.gnu.org/licenses/.

;;; Commentary:
;;
;; Provides configuration for linked-buffers where one buffer has beginning of
;; line comment characters that the other one lacks. Commented regions may be
;; in blocks with block-delimiters between then.

(require 'dash)
(require 'm-buffer)
(require 'linked-buffer)

;;; Code:

(defclass linked-buffer-block-configuration (linked-buffer-default-configuration)
  ((comment :initarg :comment
            :documentation "The comment character")
   (comment-start :initarg :comment-start
                  :documentation
                  "Demarcation for the start of the commenting region")
   (comment-stop :initarg :comment-stop
                :documentation
                "Demarcaction for the end of the commenting region."))
  :documentation "Base configuration for blocked linked-buffers.
A blocked linked-buffer is one where blocks of the buffer have a
start of line block comment in one buffer but not the other."
  :abstract t
  )


(defmethod linked-buffer-blk-comment-start-regexp
  ((conf linked-buffer-block-configuration))
  ;; todo -- what does this regexp do?
  (format "^\\(%s\\)*%s"
          (oref conf :comment)
          (regexp-quote
           (oref conf :comment-start))))

(defmethod linked-buffer-blk-comment-stop-regexp
  ((conf linked-buffer-block-configuration))
  (format "^\\(%s\\)*%s"
          (oref conf :comment)
          (regexp-quote
           (oref conf :comment-stop))))

(defmethod linked-buffer-blk-line-start-comment
  ((conf linked-buffer-block-configuration))
  (concat "^" (oref conf :comment)))

(defun linked-buffer-blk-uncomment-region (conf begin end buffer)
  "Given CONF,  remove start-of-line characters in region.
Region is between BEGIN and END in BUFFER. CONF is a
function `linked-buffer-configuration' object."
  (m-buffer-replace-match
   (m-buffer-match-data
    buffer
    (linked-buffer-blk-line-start-comment conf)
    :begin begin :end end) ""))

(defun linked-buffer-blk-uncomment-buffer (conf begin end buffer)
  "Given CONF, a `linked-buffer-configuration' object, remove all
start of line comment-characters in appropriate blocks between
BEGIN and END in BUFFER."
  (-map
   (lambda (pairs)
     (linked-buffer-blk-uncomment-region conf
      (car pairs) (cdr pairs) buffer))
   (linked-buffer-blk-marker-boundaries conf begin end buffer)))

(defun linked-buffer-blk-comment-region (conf begin end buffer)
  "Given CONF, a `linked-buffer-configuration' object, add
start of line comment characters beween BEGIN and END in BUFFER."
  (m-buffer-replace-match
   (m-buffer-match-data
    buffer
    ;; perhaps we should ignore lines which are already commented,
    "\\(^\\).+"
    :begin begin :end end)
   (oref conf :comment) 1))

(defun linked-buffer-blk-comment-buffer (conf begin end buffer)
  "Given CONF, a `linked-buffer-configuration' object, add
start of line comment-characters in appropriate blocks between
BEGIN and END in BUFFER."
  (-map
   ;; comment each of these regions
   (lambda (pairs)
     (linked-buffer-blk-comment-region
      conf (car pairs) (cdr pairs) buffer))
   (linked-buffer-blk-marker-boundaries conf begin end buffer)))

(put 'unmatched-delimiter-error
     'error-conditions
     '(error unmatched-delimiter-error))

(put 'unmatched-delimiter-error
     'error-message "Unmatched Delimiter in Buffer")

(defun linked-buffer-blk-marker-boundaries (conf begin end buffer)
  "Given CONF, a `linked-buffer-configuration' object, find
demarcation markers between BEGIN and END in BUFFER. Returns a
list of start end cons pairs. BEGIN is considered to be an
implicit start and END an implicit stop."
  (let ((match-start
         (m-buffer-match-
          buffer
          (linked-buffer-block-comment-start-regexp conf)))
        (match-end
         (m-buffer-match-end
          buffer
          (linked-buffer-block-comment-stop-regexp conf))))
    (unless
        (= (length match-start)
           (length match-end))
      (linked-buffer-log "delimiters do not match")
      (signal 'unmatched-delimiter-error
              (list begin end buffer)))
    (-zip
     ;; start comment markers
     ;; plus the start of the region
     (cons
      (set-marker (make-marker) begin buffer)
      match-start)
     ;; end comment markers
     ;; plus the end of the buffer
     (append
      match-end
      (list (set-marker (make-marker) end buffer))))))

(defun linked-buffer-pabbrev-expansion-length ()
  "Returns the length of any text that pabbrev has currently added to the buffer."
  ;; this *exact* form suppresses byte compiler warnings.
  ;; when or if and does not!
  (if (boundp 'pabbrev-expansion)
      (if pabbrev-expansion
          ;; pabbrev sorts the expansion but also adds "[]" either side"
          (+ 2 (length pabbrev-expansion))
        0)))

(defmethod linked-buffer-convert ((conf linked-buffer-block-configuration)
                                  location)
  "Converts a LOCATION in buffer FROM into one from TO.
This uses a simple algorithm; we pick the same line and then
count from the end, until we get to location, always staying on
the same line. This works since the buffers are identical except
for changes to the beginning of the line. It is also symmetrical
between the two buffers; we don't care which one has comments."
  (let ((line-plus
         (with-current-buffer
             (linked-buffer-this conf)
           (list
            (line-number-at-pos location)
            (- (line-end-position)
               ;; pabbrev adds text to the buffer, but doesn't signal a
               ;; modification (if it does, it causes the linked buffer to
               ;; show modification when it adds overlays), so it doesn't get
               ;; copied to the TO buffer. This expansion adds to the
               ;; line-end-position in the FROM buffer. So, we need to take
               ;; this length of, or the point will be too far forward in the
               ;; TO buffer.
               (linked-buffer-pabbrev-expansion-length)
               location)))))
    (with-current-buffer
        (linked-buffer-that conf)
      (save-excursion
        (goto-char (point-min))
        (forward-line (1- (car line-plus)))
        (max (line-beginning-position)
             (- (line-end-position)
                (cadr line-plus)))))))


(defclass linked-buffer-commented-block-configuration
  (linked-buffer-block-configuration)
  ()
  "Configuration for blocked linked-buffer with comments.")

(defclass linked-buffer-uncommented-block-configuration
  (linked-buffer-block-configuration)
  ()
  "Configuration for blocked linked-buffer without comments.")

(defmethod linked-buffer-clone
  ((conf linked-buffer-commented-block-configuration))
  "Update the contents in the linked-buffer without comments"
  (linked-buffer-log "blk-clone-uncomment (from):(%s)" conf)
  ;; clone the buffer first
  (call-next-method conf)
  ;; remove the line comments in the to buffer
  ;; if the delimitors are unmatched, then we can do nothing other than clone.
  (condition-case e
      (linked-buffer-blk-uncomment-buffer
       conf (point-min) (point-max) (linked-buffer-that conf))
    (unmatched-delimiter-error
     nil)))

(defmethod linked-buffer-invert
  ((conf linked-buffer-commented-block-configuration))
  (linked-buffer-uncommented-block-configuration
   "commented-inverted"
   :this-buffer (oref conf :that-buffer)
   :that-buffer (oref conf :this-buffer)
   :comment (oref conf :comment)
   :comment-start (oref conf :comment-start)
   :comment-stop (oref conf :comment-stop)))

(defmethod linked-buffer-block-comment-start-regexp
  ((conf linked-buffer-commented-block-configuration))
  (concat (regexp-quote (oref conf :comment))
          (oref conf :comment-start)))

(defmethod linked-buffer-block-comment-stop-regexp
  ((conf linked-buffer-commented-block-configuration))
  (concat (regexp-quote (oref conf :comment))
          (oref conf :comment-stop)))

(defmethod linked-buffer-clone
  ((conf linked-buffer-uncommented-block-configuration))
  "Update the contents in the linked-buffer with comments."
  (linked-buffer-log "blk-clone-comment conf):(%s)" conf)
  (call-next-method conf)
  (condition-case e
      (linked-buffer-blk-comment-buffer
       conf (point-min) (point-max) (linked-buffer-that conf))
    (unmatched-delimiter-error nil)))

(defmethod linked-buffer-invert
  ((conf linked-buffer-uncommented-block-configuration))
  (linked-buffer-commented-block-configuration
   "uncommented-inverted"
   :this-buffer (oref conf :that-buffer)
   :that-buffer (oref conf :this-buffer)
   :comment (oref :comment conf)
   :comment-start (oref :comment-start conf)
   :comment-stop (oref :comment-stop conf)))

(defmethod linked-buffer-block-comment-start-regexp
  ((conf linked-buffer-uncommented-block-configuration))
  (oref conf :comment-start))

(defmethod linked-buffer-block-comment-stop-regexp
  ((conf linked-buffer-uncommented-block-configuration))
  (oref conf :comment-stop))

(provide 'linked-buffer-block)
;;; linked-buffer-block.el ends here
