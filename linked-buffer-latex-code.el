;; linked-buffer-latex-code.el -- Latex literate programming -*- lexical-binding: t -*-
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
;; A `linked-buffer-block-configuration' environment where one buffer is latex
;; and the other is some programming language, with code blocks marked up with
;; a \begin{code}\end{code} environment.
;;
;; The code environment is not normally defined and has been picked for this
;; reason. It avoids defining multiple init functions for different macros;
;; instead the code blocks can be interpreted using what ever environment the
;; author wants, by defining the code environment first.

;;; Code:
(require 'linked-buffer-block)

(defun linked-buffer-clojure-latex-init ()
  (setq linked-buffer-config
        (linked-buffer-commented-block-configuration
         "lb-commented"
         :this-buffer (current-buffer)
         :linked-mode 'tex-mode
         :linked-file
         (concat
          (file-name-sans-extension
           (buffer-file-name)) ".tex")
         :comment ";; "
         :comment-start "\\\\end{code}"
         :comment-stop "\\\\begin{code}")))

(provide 'linked-buffer-latex-code)

;;; linked-buffer-latex-code ends here
