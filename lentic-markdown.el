;;; lentic-markdown.el -- Markdown literate programming -*- lexical-binding: t -*-

;;; Header:

;; This file is not part of Emacs

;; Author: Phillip Lord <phillip.lord@russet.org.uk>
;; Maintainer: Phillip Lord <phillip.lord@russet.org.uk>
;; Version: 0.1

;; The contents of this file are subject to the GPL License, Version 3.0.

;; Copyright (C) 2014-2022  Free Software Foundation, Inc.

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

;; A `lentic-chunk-configuration' environment where one buffer is markdown
;; and the other is some programming language. Only fenced code blocks
;; are supported as indentation based blocks is challenging.

;;; Code:
(require 'lentic-chunk)

(defun lentic-markdown-clojure-oset (conf)
  (lentic-m-oset
   conf
   'this-buffer (current-buffer)
   'comment ";; "
   'comment-start "```$"
   'comment-stop "```{.*}$"))

(defun lentic-clojure-to-markdown-new ()
  (lentic-markdown-clojure-oset
   (lentic-commented-chunk-configuration
    :lentic-file
    (concat (file-name-sans-extension buffer-file-name) ".md"))))

;;;###autoload
(defun lentic-clojure-markdown-init ()
  (lentic-clojure-to-markdown-new))

(add-to-list 'lentic-init-functions #'lentic-clojure-markdown-init)

(provide 'lentic-markdown)

;; #+begin_src emacs-lisp
