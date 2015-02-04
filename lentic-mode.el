;;; lentic-mode.el --- minor mode for lentic buffers -*- lexical-binding: t -*-

;;; Header:

;; This file is not part of Emacs

;; Author: Phillip Lord <phillip.lord@newcastle.ac.uk>
;; Maintainer: Phillip Lord <phillip.lord@newcastle.ac.uk>
;; The contents of this file are subject to the GPL License, Version 3.0.

;; Copyright (C) 2014, 2015, Phillip Lord, Newcastle University

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A minor mode for creating and manipulated lentic buffers.

;;; Code:

;; ** Preliminaries

;; #+begin_src emacs-lisp
(require 'lentic)
(require 'lentic-doc)
;; #+end_src

;; ** Window and Buffer Functions

;; #+begin_src emacs-lisp
(defun lentic-mode-move-lentic-window ()
  "Move the lentic buffer into the current window.
If the lentic is currently being displayed in another window,
then the current-buffer will be moved into that window. See also
`lentic-mode-swap-buffer-windows'."
  (interactive)
  (let ((before-window-start
         (window-start (get-buffer-window)))
        (before-window-point
         (point)))
    (lentic-mode-swap-buffer-windows
     (current-buffer)
     (lentic-that lentic-config))
    (set-window-start
     (selected-window)
     before-window-start)
    (goto-char before-window-point)))

(defun lentic-mode-swap-lentic-window ()
  "Swap the window of the buffer and lentic.
If both are current displayed, swap the windows they
are displayed in, which keeping current buffer.
See also `lentic-mode-move-lentic-window'."
  (interactive)
  (lentic-mode-swap-buffer-windows
   (current-buffer)
   (lentic-that lentic-config))
  (when (window-live-p
         (get-buffer-window
          (current-buffer)))
    (select-window
     (get-buffer-window
      (current-buffer)))))

(defun lentic-mode-swap-buffer-windows (a b)
  "Swaps the window that two buffers are displayed in.
A and B are the buffers."
  (let ((window-a (get-buffer-window a))
        (window-b (get-buffer-window b)))
    (when window-a
      (set-window-buffer
       window-a b))
    (when window-b
      (set-window-buffer
       window-b a))))

(defun lentic-mode-create-in-selected-window ()
  "Create a lentic buffer and move it to the current window."
  (interactive)
  (let ((before-window-start
         (window-start (get-buffer-window)))
        (before-window-point
         (point)))
    (lentic-ensure-init)
    (set-window-buffer
     (selected-window)
     (lentic-create lentic-config))
    (set-window-start
     (selected-window)
     before-window-start)
    (goto-char before-window-point)))

(defun lentic-mode-split-window-below ()
  "Create a lentic buffer in a new window below."
  (interactive)
  (lentic-ensure-init)
  (set-window-buffer
   (split-window-below)
   (lentic-create lentic-config)))

(defun lentic-mode-split-window-right ()
  "Create a lentic buffer in a new window right."
  (interactive)
  (lentic-ensure-init)
  (set-window-buffer
   (split-window-right)
   (lentic-create lentic-config)))
;; #+end_src

;; ** Minor Mode

;; #+begin_src emacs-lisp
;;;###autoload
(defun lentic-mode-toggle-auto-sync-point ()
  (interactive)
  (lentic-when-lentic
   (oset lentic-config :sync-point
         (not (oref lentic-config :sync-point)))))

(defvar lentic-mode-map (make-sparse-keymap)
  "Keymap for lentic-minor-mode")

(define-key lentic-mode-map
  (kbd "C-c ,s") 'lentic-mode-swap-lentic-window)

(define-key lentic-mode-map
  (kbd "C-c ,h") 'lentic-mode-move-lentic-window)

(define-key lentic-mode-map
  (kbd "C-c ,b") 'lentic-mode-split-window-below)

(define-key lentic-mode-map
  (kbd "C-c ,t") 'lentic-mode-split-window-right)

(define-key lentic-mode-map
  (kbd "C-c ,f") 'lentic-mode-insert-file-local)

(define-key lentic-mode-map
  (kbd "C-c ,c") 'lentic-mode-create-in-selected-window)

(defcustom lentic-mode-line-lighter "Lentic"
  "Default mode lighter for lentic"
  :group 'lentic
  :type 'string)

(defvar-local lentic-mode-line (format " %s[]" lentic-mode-line-lighter))

(defun lentic-mode-update-mode-line ()
  (setq lentic-mode-line
        (format " %s[%s]"
                lentic-mode-line-lighter
                (if lentic-config
                    (lentic-mode-line-string lentic-config)
                  "")))
  (force-mode-line-update))

(defun lentic-mode-buffer-list-update-hook ()
  (-map
   (lambda (b)
     (with-current-buffer
         b
       (lentic-mode-update-mode-line)))
   (buffer-list)))

(add-hook 'buffer-list-update-hook
         'lentic-mode-buffer-list-update-hook)

;;;###autoload
(define-minor-mode lentic-mode
  "Documentation"
  :lighter lentic-mode-line
  :keymap lentic-mode-map)

;;;###autoload
(easy-menu-change
 '("Edit")
 "Lentic"
 '(["Create Here" lentic-mode-create-in-selected-window
    :active (not lentic-config)]
   ["Split Below" lentic-mode-split-window-below
    :active (not lentic-config)]
   ["Split Right" lentic-mode-split-window-right
    :active (not lentic-config)]
   ["Insert File Local" lentic-mode-insert-file-local
    :active (not lentic-config)]
   ["Move Here" lentic-mode-move-lentic-window :active lentic-config]
   ["Swap" lentic-mode-swap-lentic-window :active lentic-config]
   ["Read Doc (eww)" lentic-doc-eww-view]
   ["Read Doc (external)" lentic-doc-external-view]
   ))

;;;###autoload
(defun lentic-mode-insert-file-local (init-function)
  (interactive
   (list (completing-read
          "Lentic init function: "
          (mapcar
           'symbol-name
           lentic-init-functions)
          'identity 'confirm)))
  (save-excursion
    (goto-char (point-max))
    (let ((start (point)))
      (insert
       (format
        ;; split this string or we get local variable not terminated properly
        ;; errors.
        (concat "\nLocal"
                " Variables:\nlentic-init: %s\nEnd:\n") init-function))
      (comment-region start (point)))))

;;;###autoload
(define-globalized-minor-mode global-lentic-mode
  lentic-mode
  lentic-mode-on)

(defun lentic-mode-on ()
  (lentic-mode 1))

;; #+end_src

(provide 'lentic-mode)

;;; lentic-mode.el ends here
