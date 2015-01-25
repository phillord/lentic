;;; lentic-block.el --- Comment blocks in one buffer -*- lexical-binding: t -*-

;;; Header:

;; This file is not part of Emacs

;; Author: Phillip Lord <phillip.lord@newcastle.ac.uk>
;; Maintainer: Phillip Lord <phillip.lord@newcastle.ac.uk>

;; The contents of this file are subject to the LGPL License, Version 3.0.

;; Copyright (C) 2014, Phillip Lord, Newcastle University

;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License
;; for more details.

;; You should have received a copy of the GNU Lesser General Public License
;; along with this program. If not, see http://www.gnu.org/licenses/.


;;; Commentary:

;; Lentic-block provides support for editing lentic buffers where there are large
;; documentation blocks in one view which must be commented out in the other,
;; where the blocks are demarked with some kind of delimitor.

;; This form is generally useful for forms of literate programming. For example,
;; we might embed Emacs-Lisp within LaTeX like so:

;; #+BEGIN_EXAMPLE
;; \begin{code}
;; (message "hello")
;; \end{code}
;; #+END_EXAMPLE

;; In this case, the =\begin{code}= macro defines the start of the code block. In
;; the code-centric view any lines not enclosed by the markers will be
;; commented-out, ensure that the documentation does not interfere with whatever
;; programming language is being used.

;; The implementation provided here is reasonably efficient, with only small
;; change regions being percolated.

;; This package does not provide any direct end-user configurations. These are
;; provided elsewhere.

;;; Code:

;; The implementation

;; ** Configuration Options

;; #+begin_src emacs-lisp
(require 'm-buffer)
(require 'm-buffer-at)
(require 'lentic)

(defclass lentic-block-configuration (lentic-default-configuration)
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
  :documentation "Base configuration for blocked lentics.
A blocked lentic is one where blocks of the buffer have a
start of line block comment in one buffer but not the other."
  :abstract t)

(defmethod lentic-blk-comment-start-regexp
  ((conf lentic-block-configuration))
  ;; todo -- what does this regexp do?
  (format "^\\(%s\\)*%s"
          (oref conf :comment)
          (regexp-quote
           (oref conf :comment-start))))

(defmethod lentic-blk-comment-stop-regexp
  ((conf lentic-block-configuration))
  (format "^\\(%s\\)*%s"
          (oref conf :comment)
          (regexp-quote
           (oref conf :comment-stop))))

(defmethod lentic-blk-line-start-comment
  ((conf lentic-block-configuration))
  (concat "^"
          (oref conf :comment)))

(defun lentic-blk-uncomment-region (conf begin end buffer)
  "Given CONF,  remove start-of-line characters in region.
Region is between BEGIN and END in BUFFER. CONF is a
function `lentic-configuration' object."
  ;;(lentic-log "uncomment-region (%s,%s)" begin end)
  (m-buffer-with-markers
      ((comments
        (m-buffer-match
         buffer
         (lentic-blk-line-start-comment conf)
         :begin begin :end end)))
    (m-buffer-replace-match comments "")))

(defun lentic-blk-uncomment-buffer (conf begin end buffer)
  "Given CONF, a `lentic-configuration' object, remove all
start of line comment-characters in appropriate blocks. Changes
should only have occurred between BEGIN and END in BUFFER."
  (-map
   (lambda (pairs)
     ;; nil markers off as we go
     (m-buffer-with-markers
         ((block-begin (car pairs))
          (block-end (cdr pairs)))
       (when
           (and (>= end block-begin)
                (>= block-end begin))
         (lentic-blk-uncomment-region
          conf block-begin block-end buffer))))
   (lentic-blk-marker-boundaries
    conf buffer)))

(defun lentic-blk-comment-region (conf begin end buffer)
  "Given CONF, a `lentic-configuration' object, add
start of line comment characters beween BEGIN and END in BUFFER."
  (lentic-log "comment-region (%s,%s,%s)" begin end buffer)
  (m-buffer-with-markers
      ((line-match
        (m-buffer-match
         buffer
         "\\(^\\).+$"
         :begin begin :end end))
       (comment-match
        (m-buffer-match
         buffer
         ;; start to end of line which is what this regexp above matches
         (concat
          (lentic-blk-line-start-comment conf)
          ".*")
         :begin begin :end end)))
    (m-buffer-replace-match
     (m-buffer-match-exact-subtract line-match comment-match)
     (oref conf :comment) nil nil 1)))

(defun lentic-blk-comment-buffer (conf begin end buffer)
  "Given CONF, a `lentic-configuration' object, add
start of line comment-characters. Changes should only have occurred
between BEGIN and END in BUFFER."
  ;; we need these as markers because the begin and end position need to
  ;; move as we change the buffer, in the same way that the marker boundary
  ;; markers do.
  (m-buffer-with-markers
      ((begin (set-marker (make-marker) begin buffer))
       (end (set-marker (make-marker) end buffer)))
    (-map
     ;; comment each of these regions
     (lambda (pairs)
       (m-buffer-with-markers
           ((block-begin (car pairs))
            (block-end (cdr pairs)))
         (when
             (and (>= end block-begin)
                  (>= block-end begin))
           (lentic-blk-comment-region
            conf (car pairs) (cdr pairs) buffer))))
     (lentic-blk-marker-boundaries conf buffer))))

(put 'unmatched-delimiter-error
     'error-conditions
     '(error unmatched-delimiter-error))

(put 'unmatched-delimiter-error
     'error-message "Unmatched Delimiter in Buffer")

(defun lentic-blk-marker-boundaries (conf buffer)
  "Given CONF, a `lentic-configuration' object, find
demarcation markers. Returns a list of start end cons pairs.
`point-min' is considered to be an implicit start and `point-max'
an implicit stop."
  (let* ((match-block
          (lentic-block-match
           conf buffer))
         (match-start
          (car match-block))
         (match-end
          (cadr match-block)))
    (unless
        (= (length match-start)
           (length match-end))
      (lentic-log "delimiters do not match")
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

(defmethod lentic-block-match ((conf lentic-block-configuration)
                                      buffer)
  (list
   (m-buffer-match-begin
    buffer
    (lentic-block-comment-start-regexp conf)
    :case-fold-search (oref conf :case-fold-search))
   (m-buffer-match-end
    buffer
    (lentic-block-comment-stop-regexp conf)
    :case-fold-search (oref conf :case-fold-search))))

(defmethod lentic-convert ((conf lentic-block-configuration)
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
             (lentic-this conf)
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
        (lentic-that conf)
      (save-excursion
        (goto-char (point-min))
        ;; move forward to the line in question
        (forward-line (1- (car line-plus)))
        ;; don't move from the line in question
        (max (line-beginning-position)
             ;; but move in from the end
             (- (line-end-position)
                (cadr line-plus)))))))


(defclass lentic-commented-block-configuration
  (lentic-block-configuration)
  ()
  "Configuration for blocked lentic with comments.")

(defclass lentic-uncommented-block-configuration
  (lentic-block-configuration)
  ()
  "Configuration for blocked lentic without comments.")

(defmethod lentic-clone
  ((conf lentic-commented-block-configuration)
   &optional start stop length-before start-converted stop-converted)
  "Update the contents in the lentic without comments"
  ;;(lentic-log "blk-clone-uncomment (from):(%s)" conf)
  (let*
      ;; we need to detect whether start or stop are in the comment region at
      ;; the beginning of the file. We check this by looking at :that-buffer
      ;; -- if we are in the magic region, then we must be at the start of
      ;; line. In this case, we copy the entire line as it is in a hard to
      ;; predict state. This is slightly over cautious (it also catches first
      ;; character), but this is safe, it only causes the occasional
      ;; unnecessary whole line copy. In normal typing "whole line" will be
      ;; one character anyway
      ((start-in-comment
        (when
            (and start
                 (m-buffer-at-bolp
                  (lentic-that conf)
                  start-converted))
          (m-buffer-at-line-beginning-position
           (lentic-this conf)
           start)))
       (start (or start-in-comment start))
       (start-converted
        (if start-in-comment
          (m-buffer-at-line-beginning-position
           (lentic-that conf)
           start-converted)
          start-converted))
       ;; likewise for stop
       (stop-in-comment
        (when
            (and stop
                 (m-buffer-at-bolp
                  (lentic-that conf)
                  stop-converted))
          (m-buffer-at-line-end-position
              (lentic-this conf)
              stop)))
       (stop (or stop-in-comment stop))
       (stop-converted
        (if stop-in-comment
            (m-buffer-at-line-end-position
                (lentic-that conf)
                stop-converted)
          stop-converted)))
    ;; log when we have gone long
    (if (or start-in-comment stop-in-comment)
        (lentic-log "In comment: %s %s"
                           (when start-in-comment
                             "start")
                           (when stop-in-comment
                             "stop")))
    ;; now clone the buffer
    (call-next-method conf start stop length-before
                      start-converted stop-converted)
    ;; remove the line comments in the to buffer
    ;; if the delimitors are unmatched, then we can do nothing other than clone.
    (condition-case e
        (lentic-blk-uncomment-buffer
         conf
         ;; the buffer at this point has been copied over, but is in an
         ;; inconsistent state (because it may have comments that it should
         ;; not). Still, the convertor should still work because it counts from
         ;; the end
         (lentic-convert
          conf
          ;; point-min if we know nothing else
          (or start (point-min)))
         (lentic-convert
          conf
          ;; if we have a stop
          (if stop
              ;; take stop (if we have got longer) or
              ;; start length before (if we have got shorter)
              (max stop
                   (+ start length-before))
            (point-max)))
         (lentic-that conf))
      (unmatched-delimiter-error
       nil))))

(defmethod lentic-invert
  ((conf lentic-commented-block-configuration))
  (lentic-uncommented-block-configuration
   "commented-inverted"
   :this-buffer (lentic-that conf)
   :that-buffer (lentic-this conf)
   :comment (oref conf :comment)
   :comment-start (oref conf :comment-start)
   :comment-stop (oref conf :comment-stop)))

(defmethod lentic-block-comment-start-regexp
  ((conf lentic-commented-block-configuration))
  (concat
   "\\(" (regexp-quote (oref conf :comment)) "\\)?"
   (oref conf :comment-start)))

(defmethod lentic-block-comment-stop-regexp
  ((conf lentic-commented-block-configuration))
  (concat
   "\\(" (regexp-quote (oref conf :comment)) "\\)?"
   (oref conf :comment-stop)))

(defmethod lentic-clone
  ((conf lentic-uncommented-block-configuration)
   &optional start stop length-before start-converted stop-converted)
  "Update the contents in the lentic with comments."
  ;;(lentic-log "blk-clone-comment conf):(%s)" conf)
  (let*
      ((start-at-bolp
        (when
            (and start
                 (m-buffer-at-bolp
                  (lentic-this conf)
                  start))
          (m-buffer-at-line-beginning-position
              (lentic-that conf)
              start-converted)))
       (start-converted (or start-at-bolp start-converted)))
    (if (or start-at-bolp)
        (lentic-log "In comment: %s"
                           (when start-at-bolp
                             "start")))
    (call-next-method conf start stop length-before
                      start-converted stop-converted)
    (condition-case e
        (lentic-blk-comment-buffer
         conf
         ;; the buffer at this point has been copied over, but is in an
         ;; inconsistent state (because it may have comments that it should
         ;; not). Still, the convertor should still work because it counts from
         ;; the end
         (lentic-convert
          conf
          ;; point-min if we know nothing else
          (or start (point-min)))
         (lentic-convert
          conf
          ;; if we have a stop
          (if stop
              ;; take stop (if we have got longer) or
              ;; start length before (if we have got shorter)
              (max stop
                   (+ start length-before))
            (point-max)))
         (lentic-that conf))
      (unmatched-delimiter-error nil))))

(defmethod lentic-invert
  ((conf lentic-uncommented-block-configuration))
  (lentic-commented-block-configuration
   "uncommented-inverted"
   :this-buffer (lentic-that conf)
   :that-buffer (lentic-this conf)
   :comment (oref conf :comment)
   :comment-start (oref  conf :comment-start)
   :comment-stop (oref conf :comment-stop)))

(defmethod lentic-block-comment-start-regexp
  ((conf lentic-uncommented-block-configuration))
  (oref conf :comment-start))

(defmethod lentic-block-comment-stop-regexp
  ((conf lentic-uncommented-block-configuration))
  (oref conf :comment-stop))

(provide 'lentic-block)

;;; lentic-block.el ends here

;; #+end_src
