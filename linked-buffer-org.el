;;; linked-buffer-org.el --- org support for linked-buffer -*- lexical-binding: t -*-

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
;; Linked buffers with org BEGIN_SRC blocks

;; Again, the problem is switching into emacs-lisp modes, because of the
;; meaningful first line -- for example for lexical-binding which pretty much
;; MUST be at the start of the file.

;; Worse, it's not just the first line. The header of the Lisp file is very
;; structured. Really we would want to translate this into org code also (from
;; Commentary onward.

(require 'linked-buffer-block)

;;; Code:

;; Simple org to el with nothing special
(defun linked-buffer-org-to-el-new ()
  (linked-buffer-uncommented-block-configuration
   "lb-org-to-el"
   :this-buffer (current-buffer)
   :linked-file
   (concat
    (file-name-sans-extension
     (buffer-file-name))
    ".el")
   :comment ";; "
   :comment-stop "#\\\+BEGIN_SRC emacs-lisp"
   :comment-start "#\\\+END_SRC"))

(defun linked-buffer-org-el-init ()
  (setq linked-buffer-config
        (linked-buffer-org-to-el-new)))

(add-to-list 'linked-buffer-init-functions
             'linked-buffer-org-el-init)

(defun linked-buffer-el-to-org-new ()
  (linked-buffer-commented-block-configuration
   "lb-el-to-org"
   :this-buffer (current-buffer)
   :linked-file
   (concat
    (file-name-sans-extension
     (buffer-file-name))
    ".org")
   :comment ";; "
   :comment-stop "#\\\+BEGIN_SRC emacs-lisp"
   :comment-start "#\\\+END_SRC"))

(defun linked-buffer-el-org-init ()
  (setq linked-buffer-config
        (linked-buffer-el-to-org-new)))

(add-to-list 'linked-buffer-init-functions
             'linked-buffer-el-org-init)

;; this is the more complex stuff for orgel
;; which treats ;;; comments specially
(defclass linked-buffer-org-to-orgel-configuration
  (linked-buffer-uncommented-block-configuration)
  ())

(defmethod linked-buffer-clone
  ((conf linked-buffer-org-to-orgel-configuration))
  ;; do everything else to the buffer
  (call-next-method conf)
  (m-buffer-replace-match
   (m-buffer-match
    (linked-buffer-that conf)
    ";; # # "
    :end
    (cadr
     (car
      (m-buffer-match-line
       (linked-buffer-that conf)))))
   ";;; ")
  ;; replace big headers
  (m-buffer-replace-match
   (m-buffer-match (linked-buffer-that conf)
                   "^;; [*] \\(\\\w*\\)")
   ";;; \\1:"))

(defmethod linked-buffer-invert
  ((conf linked-buffer-org-to-orgel-configuration))
  (let ((rtn
         (linked-buffer-orgel-to-org-new)))
    (oset rtn :that-buffer
          (linked-buffer-this conf))
    rtn))

(defun linked-buffer-org-to-orgel-new ()
  (linked-buffer-org-to-orgel-configuration
   "lb-orgel-to-el"
   :this-buffer (current-buffer)
   :linked-file
   (concat
    (file-name-sans-extension
     (buffer-file-name))
    ".el")
   :comment ";; "
   :comment-stop "#\\\+BEGIN_SRC emacs-lisp"
   :comment-start "#\\\+END_SRC"))

(defun linked-buffer-org-orgel-init ()
  (setq linked-buffer-config
        (linked-buffer-org-to-orgel-new)))

(add-to-list 'linked-buffer-init-functions
             'linked-buffer-org-orgel-init)

(defclass linked-buffer-orgel-to-org-configuration
  (linked-buffer-commented-block-configuration)
  ())

(defmethod linked-buffer-clone
  ((conf linked-buffer-orgel-to-org-configuration))
  ;; do everything else to the buffer
  (call-next-method conf)
  (m-buffer-replace-match
   (m-buffer-match
    (linked-buffer-that conf)
    ";;; "
    :end
    (cadr
     (car
      (m-buffer-match-line
       (linked-buffer-that conf)))))
   "# # ")
  (m-buffer-replace-match
   (m-buffer-match (linked-buffer-that conf) 
                   "^;;; \\(\\\w*\\):")
   "* \\1"))

(defmethod linked-buffer-invert
  ((conf linked-buffer-orgel-to-org-configuration))
  (let ((rtn
         (linked-buffer-org-to-orgel-new)))
    (oset rtn :that-buffer (linked-buffer-this conf))
    rtn))

(defun linked-buffer-orgel-to-org-new ()
  (linked-buffer-orgel-to-org-configuration
   "lb-orgel-to-org"
   :this-buffer (current-buffer)
   ;; we don't really need a file and could cope without, but org mode assumes
   ;; that the buffer is file name bound when it exports. As it happens, this
   ;; also means that file saving is possible which in turn saves the el file
   :linked-file
   (concat
    (file-name-sans-extension
     (buffer-file-name))
    ".org")
   :comment ";; "
   :comment-stop "#\\\+BEGIN_SRC emacs-lisp"
   :comment-start "#\\\+END_SRC"))

(defun linked-buffer-orgel-org-init ()
  (setq linked-buffer-config
        (linked-buffer-orgel-to-org-new)))

(add-to-list 'linked-buffer-init-functions
             'linked-buffer-orgel-org-init)

(provide 'linked-buffer-org)
;;; linked-buffer-org.el ends here
