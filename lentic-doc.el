;;; lentic-doc.el --- Generate and View Documentation -*- lexical-binding: t -*-

;;; Header:

;; This file is not part of Emacs

;; The contents of this file are subject to the GPL License, Version 3.0.

;; Copyright (C) 2015, Phillip Lord, Newcastle University

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

;; Lentic's self-hosting documentation system.

;;; Code:

;; #+begin_src emacs-lisp
(require 'eww)
(require 'ox-html)
(require 'browse-url)
(require 'lentic)
(require 'f)
;; #+end_src

;; ** Orgify Package

;; #+begin_src emacs-lisp
(defun lentic-doc-all-files-of-package (package)
  "Fetch all the files that are part of package.
This function assumes that all the files are in one place and
follow the standard naming convention of using the package name
as a prefix. "
  (let* ((main-file
          (locate-library package))
         (dir
          (f-parent main-file))
         (others
          (f-glob
           (concat dir "/" package "*.el"))))
    others))

(defun lentic-doc-orgify-if-necessary (file)
  (let* ((target
          (concat
           (file-name-sans-extension file)
           ".org"))
         (locked
          (or (file-locked-p file)
              (file-locked-p target)))
         (open
          (or
           (get-file-buffer file)
           (get-file-buffer target))))
    (unless (or locked open)
      (when (file-newer-than-file-p file target)
        (let ((lentic-kill-retain t))
          (lentic-batch-clone-and-save-with-config
           file 'lentic-orgel-org-init))))))

(defun lentic-doc-orgify-all-if-necessary (files)
  (-map 'lentic-doc-orgify-if-necessary files))

(defun lentic-doc-orgify-package (package)
  (lentic-doc-orgify-all-if-necessary
   (lentic-doc-all-files-of-package
    (symbol-name package))))
;; #+end_src

;; ** htmlify package

;; #+begin_src: emacs-lisp
(defun lentic-doc-htmlify-package (package start)
  (lentic-doc-orgify-package package)
  (with-current-buffer
      (find-file-noselect
       (concat
        (f-parent (locate-library (symbol-name package)))
        "/"
        start))
    (let ((org-export-htmlize-generate-css 'css))
      (org-html-export-to-html))))
;; #+end_src

;; ** lentic self-doc

;; #+begin_src: emacs-lisp
;;;###autoload
(defun lentic-doc-generate-self ()
  (interactive)
  (lentic-doc-htmlify-package 'lentic "lenticular.org"))

(defvar lentic-doc-file
  (concat
   (f-parent
    (locate-library "lentic.el"))
   "/" "lenticular.html"))

(defun lentic-doc-attempt-doc-view ()
  (eww-open-file lentic-doc-file))

(defun lentic-doc-ensure-doc ()
  (unless (f-exists? lentic-doc-file)
    (lentic-doc-generate-self)))

;;;###autoload
(defun lentic-doc-eww-view ()
  (interactive)
  (lentic-doc-ensure-doc)
  (lentic-doc-attempt-doc-view))

;;;###autoload
(defun lentic-doc-external-view ()
  (interactive)
  (lentic-doc-ensure-doc)
  (browse-url-of-file lentic-doc-file))


(provide 'lentic-doc)
;; #+end_src
