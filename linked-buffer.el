;; linked-buffer.el --- One buffer as a view of another -*- lexical-binding: t -*-

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
;; Sometimes, it would be nice to edit a file in two ways at once. For
;; instance, you might have a source file in a computational language with
;; richly marked documentation. As Emacs is a modal editor, you can edit one
;; in a mode for the computational language or for the marked up
;; documentation.
;;
;; One solution to this is to use one of the multiple-mode tools which are
;; available. The problem with this is that generally need some support from
;; the modes in question. In addition, they also require tools that work with
;; the mixed content; for example, Haskells literate mode.
;;
;; Linked buffers provide an alternative solution. Two linked buffers, by
;; default, the two share content but are otherwise independent. Therefore,
;; you can have two buffers open, each showing the content in different modes;
;; to switch modes, you simply switch buffers. The content, location of point,
;; and view are shared.
;;
;; However, linked-buffers also a bi-directional transformation between the
;; two. If this is done, then the two can have different but related text. It
;; is possible to configure the transformation for any two pairs of modes.
;;
;; Main entry points are `linked-buffer-split-window-right' and
;; `linked-buffer-split-window-below' both of which create a linked buffer in
;; the new window. Programmatically `linked-buffer-create', well, creates a
;; linked-buffer.
;;
;;; Configuration:
;;
;; linked-buffers are configurable in a large number of ways. It is possible
;; to control the nature of the transformation, the default buffer name that a
;; linked-buffer takes, and the file location (or not) of the linked-buffer.
;; For this release of linked-buffer currently, each buffer can only be linked
;; to a single buffer, although this restriction will be removed in later
;; versions.
;;
;; Configuration of a buffer happens in one of two places. First,
;; `linked-buffer-init' is run when a linked-buffer is first created. This
;; function should set the actual configuration `linked-buffer-config', and is
;; mostly designed for use as a file-local variable. All subsequent
;; configuration happens through `linked-buffer-config' which is an EIEIO
;; object and associated methods.
;;
;; Currently, I have only two concrete and one abstract configurations -- one
;; which copies all text exactly, but does not transfer text-properties (which
;; indirect-buffers do). There is a block-comment configuration which is
;; designed for syntaxes where beginning on line comments are required in
;; blocks of one buffer but not in the other. Finally, there is a concrete
;; extension of the block-comment configuration which is allows transformation
;; between Clojure code and latex.
;;
;; More configurations will be forth-coming -- next on the list will be
;; Emacs-Lisp to either asciidoc or markdown, so that this source can be made
;; literate.
;;
;; I think that the current configuration scheme is good enough for the
;; future, but only time will tell and it should still be considered
;; preliminary.
;;
;;; Status:
;;
;; This is an early release partly because I am interested in comments.
;; Hopefully, it will crash rather than hang Emacs. It currently performs
;; badly on large buffers, especially changes which make many small changes.
;; There are some outstanding bugs.

;;; Code:

(require 'eieio)

(defvar linked-buffer-init 'linked-buffer-default-init
  "Function that initializes a linked-buffer. This should set up
`linked-buffer-config' appropriately and do")

;; In future versions, this may get turned into a list so that we can have
;; multiple linked buffers, but it is not clear how the user interface
;; functions such as `linked-buffer-swap-window' would work now.
(defvar linked-buffer-config nil
  "Configuration for linked-buffer.

This is a `linked-buffer-configuration' object, which defines the
way in which the text in the different buffers is kept
synchronized. This configuration is resiliant to changes of mode
in the current buffer.")

(make-variable-buffer-local 'linked-buffer-config)
(put 'linked-buffer-config 'permanent-local t)

(defun linked-buffer-config-name (buffer)
  (format "linked: %s" buffer))

;;
;; Base Configuration:

;;
(defclass linked-buffer-configuration ()
  ((this-buffer
    :initarg :this-buffer)
   (that-buffer
    :initarg :that-buffer))
  "Configuration object for linked-buffer, which defines the mechanism
by which linking happens.")

(defgeneric linked-buffer-create (conf))
(defgeneric linked-buffer-convert (conf location))
(defgeneric linked-buffer-invert (conf that-buffer))

(defmethod linked-buffer-this ((conf linked-buffer-configuration))
  (oref conf :this-buffer))

(defmethod linked-buffer-that ((conf linked-buffer-configuration))
  (oref conf :that-buffer))

(defmethod linked-buffer-ensure-that ((conf linked-buffer-configuration))
  "Get the linked-buffer for this configuration
or create it if it does not exist."
  (or (linked-buffer-that conf)
      (linked-buffer-create conf)))

;;
;; Default Configuration:
;;
;; Two buffers with exactly the same contents, like an indirect buffer
;;
(defclass linked-buffer-default-configuration (linked-buffer-configuration)
  ((linked-file
    :initform nil
    :initarg :linked-file)
   (linked-mode
    :initform 'fundamental-mode
    :initarg :linked-mode))
  "Configuration which maintains two linked-buffers with the same contents.")

(defmethod linked-buffer-create ((conf linked-buffer-default-configuration))
  "Create the linked-buffer for this configuration.
Given a `linked-buffer-configuration' object, create the linked-buffer
appropriate for that configurationuration. It is the callers
responsibility to check that buffer has not already been
created."
  ;; make sure the world is ready for linked buffers
  (linked-buffer-ensure-hooks)
  ;; create linked-buffer
  (let* ((this-buffer
          (linked-buffer-this conf))
         (that-buffer
          (get-buffer-create
           (format "*linked: %s*"
                   (buffer-name
                    this-buffer))))
         (sec-mode (oref conf :linked-mode))
         (sec-file (oref conf :linked-file)))
    ;; make sure this-buffer knows about that-buffer
    (oset conf :that-buffer that-buffer)
    ;; init that-buffer with mode, file and config
    (with-current-buffer that-buffer
      (when sec-mode
        (funcall sec-mode))
      (when sec-file
        (set-visited-file-name sec-file))
      (setq linked-buffer-config
            (linked-buffer-invert conf)))
    ;; and fix the contents
    (linked-buffer-update-contents conf)
    that-buffer))

(defmethod linked-buffer-invert ((conf linked-buffer-default-configuration))
  (linked-buffer-default-configuration
   (linked-buffer-config-name (linked-buffer-that conf))
   :this-buffer (oref conf :that-buffer)
   :that-buffer (oref conf :this-buffer)))

(defmethod linked-buffer-convert ((conf linked-buffer-default-configuration)
                                  location)
  "For this configuration, convert LOCATION to an equivalent location in
the linked-buffer."
  location)

(defmethod linked-buffer-clone ((conf linked-buffer-configuration))
  "Updates that-buffer to reflect the contents in this-buffer.

Currently, this is just a clone all method but may use regions in future."
  (with-current-buffer (oref conf :that-buffer)
    (erase-buffer)
    (insert
     (save-restriction
       (with-current-buffer (oref conf :this-buffer)
         (widen)
         (buffer-substring-no-properties
          (point-min)
          (point-max)))))))

(defun linked-buffer-default-init ()
  (setq linked-buffer-config
        (linked-buffer-default-configuration
         (linked-buffer-config-name (current-buffer))
         :this-buffer (current-buffer))))

;;
;; End the configuration section.
;;


(defmacro linked-buffer-when-linked (&rest body)
  "Evaluates body when in a linked-buffer."
  `(when (and
          linked-buffer-config
          (linked-buffer-that
           linked-buffer-config)
          (buffer-live-p
           (linked-buffer-that
            linked-buffer-config)))
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
    (condition-case-unless-debug err
        (linked-buffer-post-command-hook-1)
      (error
       (linked-buffer-hook-fail err "post-command-hook")))))

(defun linked-buffer-post-command-hook-1 ()
  (progn
    (linked-buffer-when-linked
     (linked-buffer-update-point linked-buffer-config))))

(defun linked-buffer-hook-fail (err hook)
  "Give an informative message when we have to fail."
  (message "linked-buffer mode has failed on %s hook: %s "
           hook (error-message-string err))
  (linked-buffer-emergency)
  (with-output-to-temp-buffer "*linked-buffer-fail*"
    (princ "There has been an error in linked-buffer-mode.\n")
    (princ "The following is debugging information\n\n")
    (princ (error-message-string err)))
  (select-window (get-buffer-window "*linked-buffer-fail*")))

(defun linked-buffer-swap-windows ()
  "Swaps the window that of the current buffer with that of the
first active linked-buffer."
  (interactive)
  (linked-buffer-swap-buffer-windows
   (current-buffer)
   (linked-buffer-that linked-buffer-config))
  (select-window (get-buffer-window (current-buffer))))

(defun linked-buffer-swap-buffer-windows (a b)
  "Swaps the window that two buffers are displayed in."
  (let ((window-a (get-buffer-window a))
        (window-b (get-buffer-window b)))
    (set-window-buffer
     window-a b)
    (set-window-buffer
     window-b a)))

(defun linked-buffer-ensure-init ()
  (unless (and linked-buffer-config
               (slot-boundp
                linked-buffer-config :that-buffer)
               (buffer-live-p (linked-buffer-that
                               linked-buffer-config)))
    (funcall linked-buffer-init)))

(defun linked-buffer-split-window-below ()
  "Create a linked buffer in a new window below."
  (interactive)
  (linked-buffer-ensure-init)
  (set-window-buffer
   (split-window-below)
   (linked-buffer-create linked-buffer-config)))

(defun linked-buffer-split-window-right ()
  "Create a linked buffer in a new window right"
  (interactive)
  (linked-buffer-ensure-init)
  (set-window-buffer
   (split-window-right)
   (linked-buffer-create linked-buffer-config)))

(defun linked-buffer-after-change-function (&rest rest)
  (unless linked-buffer-emergency
    (condition-case-unless-debug err
        (linked-buffer-after-change-function-1 rest)
      (error
       (linked-buffer-hook-fail err "after change")))))

(defun linked-buffer-after-change-function-1 (rest)
  (linked-buffer-when-linked
   (linked-buffer-log
    "Updating after-change (current:linked:rest): %s,%s,%s"
    (current-buffer)
    (linked-buffer-that linked-buffer-config) rest)
   (linked-buffer-update-contents linked-buffer-config)))

(defun linked-buffer-before-change-function (&rest rest)
  (unless linked-buffer-emergency
    (condition-case err
        (lambda ())
      (error
       (linked-buffer-hook-fail err "before change")))))

(defun linked-buffer-update-contents (conf)
  "Update the contents of that-buffer with the contents of this-buffer,
using conf."
  (unwind-protect
      (progn
        (setq inhibit-read-only t)
        (linked-buffer-log
         "Update config: %s" linked-buffer-config)
        (linked-buffer-clone conf))
    (setq inhibit-read-only nil)))

(defun linked-buffer-update-point (conf)
  "Update the location of point in that-buffer to reflect this-buffer.
This also attempts to update any windows so that they show the
same top-left location. "
  (let* ((from-point
          (linked-buffer-convert
           conf
           (with-current-buffer
               (linked-buffer-this conf)
             (point))))
         (from-window-start
          (linked-buffer-convert
           conf
           (window-start
            (get-buffer-window
             (linked-buffer-this conf))))))
    ;; clone point in buffer important when the buffer is NOT visible in a
    ;; window at all
    (with-current-buffer
        (linked-buffer-that conf)
      (goto-char from-point))
    ;; now clone point in all the windows that are showing the buffer
    ;; and set the start of the window which is a reasonable attempt to show
    ;; the same thing.
    (mapc
     (lambda (window)
       (with-selected-window window
         (progn
           (goto-char from-point)
           (set-window-start window from-window-start))))
     (get-buffer-window-list (linked-buffer-that conf)))))

;;
;; Test functions!
;;


(defun linked-buffer-test-after-change-function ()
  (interactive)
  (linked-buffer-after-change-function-1 nil))

(defun linked-buffer-test-post-command-hook ()
  (interactive)
  (linked-buffer-post-command-hook-1))

(defun linked-buffer-test-reinit ()
  (interactive)
  (funcall linked-buffer-init))


(provide 'linked-buffer)
