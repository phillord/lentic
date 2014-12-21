;; lentic-dev.el --- Tools for developers -*- lexical-binding: t -*-

;;; Header:

;; This file is not part of Emacs

;; Author: Phillip Lord <phillip.lord@newcastle.ac.uk>
;; Maintainer: Phillip Lord <phillip.lord@newcastle.ac.uk>
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

;; Developing support for new forms of lentic buffers is not trivial. This
;; file provides some support functions for doing so.

;;; Code:

;; #+begin_src emacs-lisp
(require 'lentic)

(defvar lentic-dev-insert-face 'font-lock-keyword-face)

(defun lentic-dev-rotate-face ()
  (interactive)
  (setq lentic-dev-insert-face
        (nth (random (length (face-list)))
             (face-list)))
  (message "Insert face is now %s"
           (propertize
            "this"
            'face
            lentic-dev-insert-face)))

(defadvice lentic-insertion-string-transform
  (after face-transform
         (string)
         disable)
  (setq ad-return-value
        (propertize
         string
         'font-lock-face
         lentic-dev-insert-face
         'face
         lentic-dev-insert-face)))

(defvar lentic-dev-enable-insertion-marking nil)
(defun lentic-dev-enable-insertion-marking ()
  (interactive)
  (if lentic-enable-insertion-marking
      (progn
        (ad-deactivate 'lentic-insertion-string-transform)
        (setq lentic-enable-insertion-marking nil)
        (message "Insertion marking off"))
    (ad-enable-advice 'lentic-insertion-string-transform
                      'after 'face-transform)
    (ad-activate 'lentic-insertion-string-transform)
    (setq lentic-enable-insertion-marking t)
    (message "Insertion marking on")))

(provide 'lentic-dev)
;; #+end_src
