;; -*- lexical-binding: t -*-
;;
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



;; A linked buffer is one that is a view of another, with exactly the same
;; text but that may be in a different major mode, and have different
;; properties.
;;

;;
;; Initial implementation was all fine, but all the changing mode stuff is
;; really scary. So markII:
;;
;; Two buffers which are permanently linked to each other with a transform
;; function between. The two are identical *except* that linked can be killed
;; indepentdly.
;;
;; Two file-local variables -- linked-buffer-mode, linked-buffer-file. mode is
;; the default mode to make a linked buffer in -- linked buffer file is the
;; default file to use (with normal-mode).
;;

;; Need to have a data structure which stores transforms for various modes.
;; Something an alist keyed on pairs from-mode to-mode, valued on
;; buffer-transform, point-transform. To be fully functional will require two
;; entries for each pair.

;; Astonishingly, it all appears to be working, at least for latex and
;; clojure. So, need to pull the block-comment in (or perhaps
;; linked-buffer-block), generalize for the delimiter and the comment. Then I
;; can change the comment to ";; " so that we keep a space. And we have it.
;;
;; After that, try it with a really really big file.

;; Need to put some hooks in, perhaps!

(defvar linked-buffer-transforms
  '(((clojure-mode latex-mode)
     (block-comment-clone-contents-with-comments
      block-comment-convert-location))
    ((latex-mode clojure-mode)
     (block-comment-clone-contents-without-comments
      block-comment-convert-location))))

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
        (progn
          (linked-buffer-when-linked
           (linked-buffer-update-point
            (current-buffer) linked-buffer-linked-buffer)))
      (error
       (linked-buffer-hook-fail err "post-command-hook")))))

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
   (linked-buffer-make (current-buffer))))

(defun linked-buffer-split-window-right ()
  "Create a linked buffer in a new window right"
  (interactive)
  (set-window-buffer
   (split-window-right)
   (linked-buffer-make (current-buffer))))

(defun linked-buffer-make (buffer)
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
    ;; lmake sure that the contents are linked!
    (linked-buffer-update-contents
     buffer lb)
    (with-current-buffer lb
      (when sec-mode
        (funcall sec-mode))
      (when sec-file
        (set-visited-file-name sec-file))
      ;; read-only, knows where it is linked from, and in the support mode.
      (setq linked-buffer-linked-buffer buffer))
    (with-current-buffer buffer
      ;; knows where we point to!
      (setq linked-buffer-linked-buffer lb))
    lb))

(defun linked-buffer-after-change-function (&rest rest)
  (unless linked-buffer-emergency
    (condition-case err
        (progn
          (linked-buffer-log
           "Updating (current:linked:rest): %s,%s,%s"
           (current-buffer) linked-buffer-linked-buffer rest)
          (linked-buffer-when-linked
           (linked-buffer-update-contents
            (current-buffer) linked-buffer-linked-buffer)))
      (error
       (linked-buffer-hook-fail err "after change")))))

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


(defun linked-buffer-content-function-for-modes
  (from-to-tuple)
  (caadr
   (assoc from-to-tuple linked-buffer-transforms)))

(defun linked-buffer-update-contents-function (from to)
  (linked-buffer-content-function-for-modes
   (linked-buffer-mode-tuple from to)))

(defun linked-buffer-convert-location-for-modes
  (from-to-tuple)
  (cadadr
   (assoc from-to-tuple linked-buffer-transforms)))

(defun linked-buffer-convert-location-function (from to)
  (linked-buffer-convert-location-for-modes
   (linked-buffer-mode-tuple from to)))

(defun linked-buffer-update-contents (from to)
  "Update the buffer using the appropriate transformation function"
  (unwind-protect
      (progn
        (setq inhibit-read-only t)
        (funcall
         (or
          (linked-buffer-update-contents-function from to)
          'linked-buffer-clone-contents)
         from to))
    (setq inhibit-read-only nil)))

(defun linked-buffer-clone-contents (from to)
  (with-current-buffer to
    (erase-buffer)
    (insert
     (save-restriction
       (with-current-buffer from
         (widen)
         (buffer-substring-no-properties
          (point-min)
          (point-max)))))))

(defun linked-buffer-update-point (from to)
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

(defun linked-buffer-default-convert-location (location from to)
  location)

(defun linked-buffer-test ()
  (interactive)
  (linked-buffer-when-linked
   (linked-buffer-update-contents
    (current-buffer) linked-buffer-linked-buffer)))

(defun test2 ()
  (interactive)
  (message "%s"
           (linked-buffer-update-contents-function
            (current-buffer) linked-buffer-linked-buffer)))


(require 'block-comment)
