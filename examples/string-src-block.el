;;; lentic_test --- testing it  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2022  Free Software Foundation, Inc.

;; #+begin_src emacs-lisp
(setq org-structure-template-alist
      (quote (("s" "#+begin_src ?\n\n#+end_src" "<src lang=\"?\">\n\n</src>")
               ("e" "#+begin_example\n?\n#+end_example" "<example>\n?\n</example>")
               ("I" "#+include %file ?" "<include file=%file markup=\"?\">"))))
;; #+end_src

;; # Local Variables:
;; # lentic-init: lentic-org-orgel-init
;; # End:
