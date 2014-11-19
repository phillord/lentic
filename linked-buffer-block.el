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
                "Demarcaction for the end of the commenting region.")
   (case-fold-search :initarg :case-fold-search
                      :documentation
                      "Should match be case sensitive"
                      :initform :default))
  :documentation "Base configuration for blocked linked-buffers.
A blocked linked-buffer is one where blocks of the buffer have a
start of line block comment in one buffer but not the other."
  :abstract t)


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
  (concat "^"
          (oref conf :comment)))

(defun linked-buffer-blk-uncomment-region (conf begin end buffer)
  "Given CONF,  remove start-of-line characters in region.
Region is between BEGIN and END in BUFFER. CONF is a
function `linked-buffer-configuration' object."
  (let
      ((comments
        (m-buffer-match
         buffer
         (linked-buffer-blk-line-start-comment conf)
         :begin begin :end end)))
    (prog1
        (m-buffer-replace-match comments "")
      (m-buffer-nil-marker comments))))

(defun linked-buffer-blk-uncomment-buffer (conf begin end buffer)
  "Given CONF, a `linked-buffer-configuration' object, remove all
start of line comment-characters in appropriate blocks. Changes
should only have occurred between BEGIN and END in BUFFER."
  (-map
   (lambda (pairs)
     (let*
         ((block-begin (car pairs))
          (block-end (cdr pairs))
          (rtn
           (when
               (and (>= end block-begin)
                    (>= block-end begin))
             (linked-buffer-blk-uncomment-region
              conf
              block-begin block-end buffer))))
       ;; remove markers as we go
       (set-marker (car pairs) nil)
       (set-marker (cdr pairs) nil)
       rtn))
   (linked-buffer-blk-marker-boundaries
    conf buffer)))

(defun linked-buffer-blk-comment-region (conf begin end buffer)
  "Given CONF, a `linked-buffer-configuration' object, add
start of line comment characters beween BEGIN and END in BUFFER."
  (linked-buffer-log "comment-region (%s,%s,%s)" begin end buffer)
  (let ((line-match
         (m-buffer-match
          buffer
          ;; perhaps we should ignore lines which are already commented,
          "\\(^\\).+$"
          :begin begin :end end))
        (comment-match
         (m-buffer-match
          buffer
          ;; start to end of line which is what this regexp above matches
          (concat
           (linked-buffer-blk-line-start-comment conf)
           ".*")
          :begin begin :end end)))
    (prog1
        (m-buffer-replace-match
         (m-buffer-match-exact-subtract line-match comment-match)
         (oref conf :comment) nil nil 1)
      (m-buffer-nil-marker line-match)
      (m-buffer-nil-marker comment-match))))

(defun linked-buffer-blk-comment-buffer (conf begin end buffer)
  "Given CONF, a `linked-buffer-configuration' object, add
start of line comment-characters. Changes should only have occurred
between BEGIN and END in BUFFER."
  ;; we need these as markers because the begin and end position need to
  ;; move as we change the buffer, in the same way that the marker boundary
  ;; markers do.
  (let* ((begin (set-marker (make-marker) begin buffer))
         (end (set-marker (make-marker) end buffer))
         (rtn
          (-map
           ;; comment each of these regions
           (lambda (pairs)
             (let* ((block-begin (car pairs))
                    (block-end (cdr pairs))
                    (rtn
                     (when
                         (and (>= end block-begin)
                              (>= block-end begin))
                       (linked-buffer-blk-comment-region
                        conf (car pairs) (cdr pairs) buffer))))
               (set-marker block-begin nil)
               (set-marker block-end nil)
               rtn))
           (linked-buffer-blk-marker-boundaries conf buffer))))
    (set-marker begin nil)
    (set-marker end nil)
    rtn))

(put 'unmatched-delimiter-error
     'error-conditions
     '(error unmatched-delimiter-error))

(put 'unmatched-delimiter-error
     'error-message "Unmatched Delimiter in Buffer")

(defun linked-buffer-blk-marker-boundaries (conf buffer)
  "Given CONF, a `linked-buffer-configuration' object, find
demarcation markers. Returns a list of start end cons pairs.
`point-min' is considered to be an implicit start and `point-max'
an implicit stop."
  (let* ((match-block
          (linked-buffer-block-match
           conf buffer))
         (match-start
          (car match-block))
         (match-end
          (cadr match-block)))
    (unless
        (= (length match-start)
           (length match-end))
      (linked-buffer-log "delimiters do not match")
      (signal 'unmatched-delimiter-error
              (list buffer)))
    (with-current-buffer buffer
      (-zip
       ;; start comment markers
       ;; plus the start of the region
       (cons
        (set-marker (make-marker) (point-min) buffer)
        match-start)
       ;; end comment markers
       ;; plus the end of the buffer
       (append
        match-end
        (list (set-marker (make-marker) (point-max) buffer)))))))

(defmethod linked-buffer-block-match ((conf linked-buffer-block-configuration)
                                      buffer)
  (list
   (m-buffer-match-begin
    buffer
    (linked-buffer-block-comment-start-regexp conf)
    :case-fold-search (oref conf :case-fold-search))
   (m-buffer-match-end
    buffer
    (linked-buffer-block-comment-stop-regexp conf)
    :case-fold-search (oref conf :case-fold-search))))

(defmethod linked-buffer-convert ((conf linked-buffer-block-configuration)
                                  location)
  "Converts a LOCATION in buffer FROM into one from TO.
This uses a simple algorithm; we pick the same line and then
count from the end, until we get to location, always staying on
the same line. This works since the buffers are identical except
for changes to the beginning of the line. It is also symmetrical
between the two buffers; we don't care which one has comments."
  ;; current information comes inside a with-current-buffer. so, we capture
  ;; data as a list rather than having two with-current-buffers.
  (let ((line-plus
         (with-current-buffer
             (linked-buffer-this conf)
           (save-excursion
             ;; move to location or line-end-position may be wrong
             (goto-char location)
             (list
              ;; we are converting the location, so we need the line-number
              (line-number-at-pos location)
              ;; and the distance from the end
              (- (line-end-position)
                 location))))))
    (with-current-buffer
        (linked-buffer-that conf)
      (save-excursion
        (goto-char (point-min))
        ;; move forward to the line in question
        (forward-line (1- (car line-plus)))
        ;; don't move from the line in question
        (max (line-beginning-position)
             ;; but move in from the end
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
  ((conf linked-buffer-commented-block-configuration)
   &optional start stop length-before)
  "Update the contents in the linked-buffer without comments"
  ;;(linked-buffer-log "blk-clone-uncomment (from):(%s)" conf)
  ;; clone the buffer first
  (call-next-method conf start stop length-before)
  ;; remove the line comments in the to buffer
  ;; if the delimitors are unmatched, then we can do nothing other than clone.
  (condition-case e
      (linked-buffer-blk-uncomment-buffer
       conf
       ;; the buffer at this point has been copied over, but is in an
       ;; inconsistent state (because it may have comments that it should
       ;; not). Still, the convertor should still work because it counts from
       ;; the end
       (linked-buffer-convert
        conf
        ;; point-min if we know nothing else
        (or start (point-min)))
       (linked-buffer-convert
        conf
        ;; if we have a stop
        (if stop
            ;; take stop (if we have got longer) or
            ;; start length before (if we have got shorter)
            (max stop
                 (+ start length-before))
          (point-max)))
       (linked-buffer-that conf))
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
  (concat
   "\\(" (regexp-quote (oref conf :comment)) "\\)?"
   (oref conf :comment-start)))

(defmethod linked-buffer-block-comment-stop-regexp
  ((conf linked-buffer-commented-block-configuration))
  (concat
   "\\(" (regexp-quote (oref conf :comment)) "\\)?"
   (oref conf :comment-stop)))

(defmethod linked-buffer-clone
  ((conf linked-buffer-uncommented-block-configuration)
   &optional start stop length-before)
  "Update the contents in the linked-buffer with comments."
  ;;(linked-buffer-log "blk-clone-comment conf):(%s)" conf)
  (call-next-method conf start stop length-before)
  (condition-case e
      (linked-buffer-blk-comment-buffer
       conf
       ;; the buffer at this point has been copied over, but is in an
       ;; inconsistent state (because it may have comments that it should
       ;; not). Still, the convertor should still work because it counts from
       ;; the end
       (linked-buffer-convert
        conf
        ;; point-min if we know nothing else
        (or start (point-min)))
       (linked-buffer-convert
        conf
        ;; if we have a stop
        (if stop
            ;; take stop (if we have got longer) or
            ;; start length before (if we have got shorter)
            (max stop
                 (+ start length-before))
          (point-max)))
       (linked-buffer-that conf))
    (unmatched-delimiter-error nil)))

(defmethod linked-buffer-invert
  ((conf linked-buffer-uncommented-block-configuration))
  (linked-buffer-commented-block-configuration
   "uncommented-inverted"
   :this-buffer (oref conf :that-buffer)
   :that-buffer (oref conf :this-buffer)
   :comment (oref conf :comment)
   :comment-start (oref  conf :comment-start)
   :comment-stop (oref conf :comment-stop)))

(defmethod linked-buffer-block-comment-start-regexp
  ((conf linked-buffer-uncommented-block-configuration))
  (oref conf :comment-start))

(defmethod linked-buffer-block-comment-stop-regexp
  ((conf linked-buffer-uncommented-block-configuration))
  (oref conf :comment-stop))

(provide 'linked-buffer-block)

;;; linked-buffer-block.el ends here
