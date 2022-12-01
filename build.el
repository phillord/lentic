;; -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2022  Free Software Foundation, Inc.

(add-to-list 'load-path
             (or (if (fboundp 'macroexp-file-name) ;; Emacs≥28
                     (file-name-directory (macroexp-file-name)))
                 default-directory))

(require 'lentic-doc)
(require 'commander)

;; FIXME: Merely loading an ELisp file should never make such changes to
;; the running Emacs.

(toggle-debug-on-error)

(defun build/gen-org ()
  (interactive)
  (lentic-doc-orgify-package 'lentic))

(defun build/gen-html ()
  (interactive)
  (lentic-doc-htmlify-package 'lentic))


(commander
 (command "gen-org" "Generate org from el" build/gen-org)
 (command "gen-html" "Generate HTML from org" build/gen-html))

;; This file can't be compiled without `commander' which is not among
;; the required dependencies.
;; Local Variables:
;; no-byte-compile: t
;; End:
