;;; orgel-el-with-tags.el -- an example file  -*- lexical-binding: t; -*-

;;; Header:

;; Copyright (C) 2015-2022  Free Software Foundation, Inc.

;;; Commentary:

;; This file is a test file to see if we can have tags on headers.

;;; Code:

;; #+begin_src emacs-lisp
(message "Hello World")
;; #+end_src

;;; Hello:                                                                                :tag:




;; This is a level 1 header with a tag.

;; ** HeaderTwo                             :anothertag:

;; This is a level 2 header with a tag

;; ** HeaderTwo   Hello                                                    :bob:


;; Local Variables:
;; lentic-init: lentic-orgel-org-init
;; End:
