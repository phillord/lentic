;; linked-buffer.el --- One buffer as a view of another -*- lexical-binding: t -*-

;; This file is not part of Emacs

;; Author: Phillip Lord <phillip.lord@newcastle.ac.uk>
;; Maintainer: Phillip Lord <phillip.lord@newcastle.ac.uk>
;; Version: 0.1

;; The contents of this file are subject to the GPL License, Version 3.0.
;;
;; Copyright (C) 2014, Phillip Lord, Newcastle University
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Sometimes, it would be nice to edit a file in two ways at once. For
;; instance, you might have a source file in a computational language with
;; richly marked documentation. As Emacs is a modal editor, you can edit one
;; in a mode for the computational language or for the marked up
;; documentation.
;;
;; One solution to this is to use one of the multiple-mode tools which are
;; available. The problem with this is that generally need some support from
;; the modes in question. In addition, they also require tools that work with
;; the mixed content; for example, Haskells literate mode.
;;
;; Linked buffers provide an alternative solution. Two linked buffers, by
;; default, the two share identical content but are otherwise independent.
;; Therefore, you can have two buffers open, each showing the content in
;; different modes; to switch modes, you simply switch buffers. The content,
;; location of point, and view are shared.
;;
;; However, linked-buffers also a bi-directional transformation between the
;; two. If this is done, then the two can have different but related text. It
;; is possible to configure the transformation for any two pairs of modes.
;;
;; Main entry points are `linked-buffer-split-window-right' and
;; `linked-buffer-split-window-below' both of which create a linked buffer in
;; the new window. Programmatically `linked-buffer-create', well, creates a
;; linked-buffer.
;;
;;; Configuration:
;;
;; Currently, I have this configured only for clojure-mode and latex-mode.
;; Comments in Clojure are translated into non-commented latex. At the moment,
;; the configuration is not stable; I need to try it with a least one other
;; pair of modes. R and latex should be trivial to add. Org-mode and any of
;; the languages it supports should work also.
;;
;;; Status:
;;
;; This is an early release partly because I am interested in comments. The
;; API is open to change and it make behave badly, crash or eat your children.
;; Hopefully, it will crash rather than hang Emacs.

(require 'dash)
(require 'm-buffer)



(defvar linked-buffer-transforms
  '(
    ((clojure-mode latex-mode)
     :convert linked-buffer-blk-clone-uncomment
     :location linked-buffer-blk-convert-location
     :comment ";;"
     :comment-block-start "\\end{code}"
     :comment-block-stop "\\begin{code}")
    ((latex-mode clojure-mode)
     :convert linked-buffer-blk-clone-comment
     :location linked-buffer-blk-convert-location
     :comment ";;"
     :comment-block-start "\\end{code}"
     :comment-block-stop "\\begin{code}")))

(defvar linked-buffer-linked-buffer nil
  "The linked-buffer for this buffer")

(make-variable-buffer-local 'linked-buffer-linked-buffer)
;; protects again mode changes
(put 'linked-buffer-linked-buffer 'permanent-local t)

(defvar linked-buffer-secondary-mode nil
  "If set, any new linked-buffer will use this mode.
In general, this makes most sense when used as a file-local
variable.")

(defvar linked-buffer-secondary-file nil
  "If set, any new linked-buffer will visit this file.
Beware that the contents of the file will be replaced with the
contents of the primary file. It is save to use this as a
file-local variable.")

(make-variable-buffer-local 'linked-buffer-secondary-mode)

(defmacro linked-buffer-when-linked (&rest body)
  "Evaluates body when in a linked-buffer."
  `(when (and linked-buffer-linked-buffer
              (buffer-live-p linked-buffer-linked-buffer))
     ,@body))

(defun linked-buffer-ensure-hooks ()
  "Ensures that the hooks that this mode requires are in place"
  (add-hook 'post-command-hook
            'linked-buffer-post-command-hook)
  ;; after and before-change functions are hooks (with args) even if they are
  ;; not named as such.
  (add-hook 'after-change-functions
            'linked-buffer-after-change-function)
  (add-hook 'before-change-functions
            'linked-buffer-before-change-function))

(defvar linked-buffer-log t)
(defmacro linked-buffer-log (&rest rest)
  `(when linked-buffer-log
     (linked-buffer-when-linked
      (let ((msg
             (concat
              (format ,@rest)
              "\n")))
        (with-current-buffer
            (get-buffer-create "*linked-buffer-log*")
          (goto-char (point-max))
          (insert msg))))))

(defvar linked-buffer-emergency nil)

(defun linked-buffer-emergency ()
  "Ensures that the hooks that this mode requires are in place"
  (interactive)
  (setq linked-buffer-emergency t))

(defun linked-buffer-post-command-hook ()
  (unless linked-buffer-emergency
    (condition-case err
        (linked-buffer-post-command-hook-1)
      (error
       (linked-buffer-hook-fail err "post-command-hook")))))

(defun linked-buffer-post-command-hook-1 ()
  (progn
    (linked-buffer-when-linked
     (linked-buffer-update-point
      (current-buffer) linked-buffer-linked-buffer))))

(defun linked-buffer-hook-fail (err hook)
  "Give an informative message when we have to fail."
  (message "linked-buffer mode has failed on %s hook: %s "
           hook (error-message-string err))
  (linked-buffer-emergency)
  (with-output-to-temp-buffer "*linked-buffer-fail*"
    (princ "There has been an error in linked-buffer-mode.\n")
    (princ "The following is debugging information\n\n")
    (princ (error-message-string err))
    (princ "\n\nBacktrace is: \n" )
    (let ((standard-output (get-buffer "*linked-buffer-fail*" )))
      (backtrace)))
  (select-window (get-buffer-window "*linked-buffer-fail*")))

(defun linked-buffer-swap-windows ()
  "Swaps the window that a buffer and its linked buffer display in."
  (interactive)
  (linked-buffer-swap-buffer-windows
   (current-buffer)
   linked-buffer-linked-buffer)
  (select-window (get-buffer-window (current-buffer))))

(defun linked-buffer-swap-buffer-windows (a b)
  "Swaps the window that two buffers are displayed in."
  (let ((window-a (get-buffer-window a))
        (window-b (get-buffer-window b)))
    (set-window-buffer
     window-a b)
    (set-window-buffer
     window-b a)))

(defun linked-buffer-split-window-below ()
  "Create a linked buffer in a new window below."
  (interactive)
  (set-window-buffer
   (split-window-below)
   (linked-buffer-create (current-buffer))))

(defun linked-buffer-split-window-right ()
  "Create a linked buffer in a new window right"
  (interactive)
  (set-window-buffer
   (split-window-right)
   (linked-buffer-create (current-buffer))))

(defun linked-buffer-create (buffer)
  "Create a linked buffer for BUFFER."
  ;; make sure the world is ready for linked buffers
  (linked-buffer-ensure-hooks)
  ;; TODO  this bit needs changing!
  (let* ((lb (get-buffer-create
              (format "*linked: %s*"
                      (buffer-name buffer))))
         (sec-mode
          (with-current-buffer buffer
            linked-buffer-secondary-mode))
         (sec-file-maybe
          (with-current-buffer buffer
            linked-buffer-secondary-file))
         ;; if we open a file that has been created by linked-buffer
         ;; it may contain a file-local-variable saying that the
         ;; secondary-file is itself. If we open a linked-buffer from here, it
         ;; would have the same file as the first. Not good.
         (sec-file
          (if (eq sec-file-maybe
                  buffer-file-name)
              nil
            sec-file-maybe)))
    (with-current-buffer lb
      (when sec-mode
        (funcall sec-mode))
      (when sec-file
        (set-visited-file-name sec-file))
      ;; Now we have set up the linked-buffer with mode or file, we can update
      ;; the contents. Run update here, *before* we set the linked-buffer so
      ;; that we do not run the linked-buffer-after-change-functions
      (linked-buffer-update-contents
       buffer lb)
      (setq linked-buffer-linked-buffer buffer))
    (with-current-buffer buffer
      ;; knows where we point to!
      (setq linked-buffer-linked-buffer lb))
    lb))

(defun linked-buffer-after-change-function (&rest rest)
  (unless linked-buffer-emergency
    (condition-case err)
    (linked-buffer-after-change-function-1 rest)
    (error
     (linked-buffer-hook-fail err "after change"))))

(defun linked-buffer-after-change-function-1 (rest)
  (linked-buffer-when-linked
   (linked-buffer-log
    "Updating after-change (current:linked:rest): %s,%s,%s"
    (current-buffer) linked-buffer-linked-buffer rest)
   (linked-buffer-update-contents
    (current-buffer) linked-buffer-linked-buffer)))

(defun linked-buffer-before-change-function (&rest rest)
  (unless linked-buffer-emergency
    (condition-case err
        (lambda ())
      (error
       (linked-buffer-hook-fail err "before change")))))

(defun linked-buffer-mode-tuple (from to)
  (list
   (with-current-buffer from major-mode)
   (with-current-buffer to major-mode)))

(defun linked-buffer-config-for-modes (from-to-tuple)
  (cdr
   (assoc from-to-tuple linked-buffer-transforms)))

(defun linked-buffer-content-function-for-modes
  (from-to-tuple)
  (plist-get
   (linked-buffer-config-for-modes from-to-tuple)
   :convert))

(defun linked-buffer-update-contents-function (from to)
  (linked-buffer-content-function-for-modes
   (linked-buffer-mode-tuple from to)))

(defun linked-buffer-convert-location-for-modes
  (from-to-tuple)
  (plist-get
   (linked-buffer-config-for-modes from-to-tuple)
   :location))

(defun linked-buffer-convert-location-function (from to)
  (linked-buffer-convert-location-for-modes
   (linked-buffer-mode-tuple from to)))

(defun linked-buffer-update-contents (from to)
  "Update the contents of buffer TO to reflect the buffer FROM.
The update is performed based on the :convert value in
`linked-buffer-transforms' or
`linked-buffer-default-clone-contents' if that is nil."
  (unwind-protect
      (progn
        (setq inhibit-read-only t)
        (linked-buffer-log
         "Update function: %s"
         (or
          (linked-buffer-update-contents-function from to)
          'linked-buffer-default-clone-contents))
        (funcall
         (or
          (linked-buffer-update-contents-function from to)
          'linked-buffer-default-clone-contents)
         from to))
    (setq inhibit-read-only nil)))

(defun linked-buffer-update-point (from to)
  "Update the location of point in buffer TO to reflect the buffer FROM.
This also attempts to update any windows so that they show the
same top-left location. The update is performed based on
the :location value in `linked-buffer-transforms' or
`linked-buffer-default-convert-location' if that is nil.
"
  (let* ((convert-function
          (or (linked-buffer-convert-location-function from to)
              'linked-buffer-default-convert-location ))
         (from-point
          (funcall convert-function
                   (with-current-buffer from
                     (point))
                   from to))
         (from-window-start
          (funcall convert-function
                   (window-start
                    (get-buffer-window from))
                   from to)))
    (mapc
     (lambda (window)
       (with-selected-window window
         (progn
           (goto-char from-point)
           (set-window-start window from-window-start))))
     (get-buffer-window-list to))))


;;
;; Dumb linked buffer
;;
(defun linked-buffer-default-clone-contents (from to)
  "Updates to buffer TO to reflect the contents in FROM.
This uses a dump algorithm and just copies everything in memory
from one to the other."
  (with-current-buffer to
    (erase-buffer)
    (insert
     (save-restriction
       (with-current-buffer from
         (widen)
         (buffer-substring-no-properties
          (point-min)
          (point-max)))))))


(defun linked-buffer-default-convert-location (location from to)
  "Converts a point LOCATION in buffer FROM to one in TO.
In practice, this just returns LOCATION."
  location)

;;
;; Test functions useful for testing new convertors
;;
(defun linked-buffer-test-after-change-function ()
  (interactive)
  (linked-buffer-after-change-function-1 nil))

(defun linked-buffer-test-post-command-hook ()
  (interactive)
  (linked-buffer-post-command-hook-1))


;;
;; Block comment linked buffer
;;
;; Links two buffers which are split into regions "comments" and "code" with
;; delimiters, or code markers, between them. In one buffer, the comment
;; regions are shown with a start-of-line comment character, while in the
;; other this is removed.
;;
(defun linked-buffer-blk-comment ()
  ";; ")

(defun linked-buffer-blk-line-start-comment ()
  (concat "^" (linked-buffer-blk-comment)))

(defun linked-buffer-blk-comment-start ()
  "\\end{code}")

(defun linked-buffer-blk-comment-stop ()
  "\\begin{code}")

(defun linked-buffer-blk-comment-start-regexp ()
  (format "^\\(%s\\)*%s"
          (linked-buffer-blk-comment)
          (regexp-quote (linked-buffer-blk-comment-start))))

(defun linked-buffer-blk-comment-stop-regexp ()
  (format "^\\(%s\\)*%s"
           (linked-buffer-blk-comment)
           (regexp-quote (linked-buffer-blk-comment-stop))))

(defun linked-buffer-blk-uncomment-region (begin end buffer)
  "Between BEGIN and END in BUFFER, remove all start of line comment characters."
  (m-buffer-replace-match
   (m-buffer-match-data
    buffer
    (linked-buffer-blk-line-start-comment)
    :beginning begin :end end) ""))

(defun linked-buffer-blk-uncomment-buffer (begin end buffer)
  "Between BEGIN and END in BUFFER remove uncomment characters in
delimiter regions."
  (-map
   (lambda (pairs)
     (linked-buffer-blk-uncomment-region
      (car pairs) (cdr pairs) buffer))
   (linked-buffer-blk-marker-boundaries begin end buffer)))

(defun linked-buffer-blk-comment-region (begin end buffer)
  "Between BEGIN and END in BUFFER add comment characters"
  (m-buffer-replace-match
   (m-buffer-match-data
    buffer
    ;; perhaps we should ignore lines which are already commented,
    "\\(^\\).+"
    :beginning begin :end end)
   (linked-buffer-blk-comment) 1))

(defun linked-buffer-blk-comment-buffer (begin end buffer)
  "Between BEGIN and END in BUFFER comment regions between delimiters."
  (-map
   ;; comment each of these regions
   (lambda (pairs)
     (linked-buffer-blk-comment-region
      (car pairs) (cdr pairs) buffer))
   (linked-buffer-blk-marker-boundaries begin end buffer)))

(put 'unmatched-delimiter-error
     'error-conditions
     '(error unmatched-delimiter-error))

(put 'unmatched-delimiter-error
     'error-message "Unmatched Delimiter in Buffer")

(defun linked-buffer-blk-marker-boundaries (begin end buffer)
  "Find demarcation markers between BEGIN and END in BUFFER.
Returns a list of start end cons pairs. BEGIN is considered to
be an implicit start and END an implicit stop."
  (let ((match-start
         (m-buffer-match-beginning
          buffer
          (linked-buffer-blk-comment-start-regexp)))
        (match-end
         (m-buffer-match-end
          buffer
          (linked-buffer-blk-comment-stop-regexp))))
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

(defun linked-buffer-blk-clone-uncomment (from to)
  "Update the contents in buffer TO to match FROM and remove comments."
  (linked-buffer-log "blk-clone-uncomment (from,to):(%s,%s)" from to)
  (linked-buffer-default-clone-contents from to)
  ;; remove the line comments in the to buffer
  ;; if the delimitors are unmatched, then we can do nothing other than clone.
  (condition-case e
      (linked-buffer-blk-uncomment-buffer (point-min) (point-max) to)
    (unmatched-delimiter-error
     nil)))

(defun linked-buffer-blk-clone-comment (from to)
  "Update the contents in buffer TO to match FROM and add comments."
  (linked-buffer-log "blk-clone-comment (from,to):(%s,%s)" from to)
  (linked-buffer-default-clone-contents from to)
  (condition-case e
      (linked-buffer-blk-comment-buffer (point-min) (point-max) to)
    (unmatched-delimiter-error nil)))

(defun linked-buffer-pabbrev-expansion-length ()
  "Returns the length of any text that pabbrev has currently added to the buffer."
  ;; this *exact* form suppresses byte compiler warnings.
  ;; when or if and does not!
  (if (boundp 'pabbrev-expansion)
      (if pabbrev-expansion
          ;; pabbrev sorts the expansion but also adds "[]" either side"
          (+ 2 (length pabbrev-expansion))
        0)))

(defun linked-buffer-blk-convert-location (location from to)
  "Converts a LOCATION in buffer FROM into one from TO.
This uses a simple algorithm; we pick the same line and then
count from the end, until we get to location, always staying on
the same line. This works since the buffers are identical except
for changes to the beginning of the line.
"
  (let ((line-plus
         (with-current-buffer from
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
        to
      (save-excursion
        (goto-line (car line-plus))
        (max (line-beginning-position)
             (- (line-end-position)
                (cadr line-plus)))))))

(provide 'linked-buffer)
