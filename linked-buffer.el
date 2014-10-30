;;; linked-buffer.el --- One buffer as a view of another -*- lexical-binding: t -*-

;;; Header:

;; This file is not part of Emacs

;; Author: Phillip Lord <phillip.lord@newcastle.ac.uk>
;; Maintainer: Phillip Lord <phillip.lord@newcastle.ac.uk>
;; Version: 0.6
;; Package-Requires: ((emacs "24")(m-buffer "0.5")(dash "2.5.0"))

;; The contents of this file are subject to the GPL License, Version 3.0.

;; Copyright (C) 2014, Phillip Lord, Newcastle University

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

;; Linked-buffers enables simultaneous editing and viewing of the same (or
;; closely related) text in two or more buffers, potentially in different modes.

;; Sometimes, it would be nice to edit a file in two ways at once. For instance,
;; you might have a source file in a computational language with richly marked
;; documentation. As Emacs is a modal editor, it would be nice to edit this file
;; both in a mode for the computational language and for the marked up
;; documentation. 

;; One solution to this is to use a single-mode which supports both types of
;; editing. The problem with this is that it is fundamentally difficult to
;; support two types of editing at the same time; more over, you need a new mode
;; for environment.

;; Another solution is to use one of the multiple-mode tools which are available.
;; The problem with this is that they generally need some support from the modes
;; in question. And, again, the difficulty is supporting both forms of editing in
;; the same environment.

;; Linked buffers provide an alternative solution. Two linked buffers, by
;; default, the two share content but are otherwise independent. Therefore,
;; you can have two buffers open, each showing the content in different modes;
;; to switch modes, you simply switch buffers. The content, location of point,
;; and view are shared.

;; However, linked-buffers can also perform a bi-directional transformation
;; between the two. If this is done, then the two can have different but related
;; text. It is possible to configure the transformation for any two buffers in a
;; extensible way, although mostly we have concentrated on mode-specific
;; configuration.

;; The main user entry point is through `global-linked-buffer-start-mode' which
;; provides tools to create new a new linked-buffer.

;;; Configuration:

;; linked-buffers are configurable in a large number of ways. It is possible
;; to control the nature of the transformation, the default buffer name that a
;; linked-buffer takes, and the file location (or not) of the linked-buffer.
;; For this release of linked-buffer currently, each buffer can only be linked
;; to a single buffer, although this restriction will be removed in later
;; versions.

;; Configuration of a buffer happens in one of two places. First,
;; `linked-buffer-init' is run when a linked-buffer is first created. This
;; function should set the actual configuration `linked-buffer-config', and is
;; mostly designed for use as a file-local or dir-local variable. All subsequent
;; configuration happens through `linked-buffer-config' which is an EIEIO object
;; and associated methods.

;; There are now a number of different configurations, which can be used for
;; general-purpose use as well as an extension points for subclass
;; configurations. The two most general configurations are:

;;  - default: this copies all text exactly, but does not transfer
;;    text-properties (which is the behaviour of indirect buffers). It is
;;    possible to configure the default file or mode on a per-object basis.
;;  - block: this is designed for programmatic syntaxes where blocks of code are
;;    demarcated by start and end tags, and everything else is commented by
;;    line-start comments. Comments are added or removed between the two buffers.

;; The second of these is extended in linked-buffer-org.el to provide the
;; configuration for this file: there is a normal emacs-lisp file in one buffer
;; and an org-mode version in another. Other programmatic and documentation modes
;; are supported in other files.

;;; Status:

;; This is an early release partly because I am interested in comments.
;; There are still bugs and it can perform badly and destructively, particularly
;; on buffers which are ill-formed with respect to their expected syntax.

;; Although it is still too early to guarantee, I hope that the current
;; configuration scheme will remain fixed, and subclass extensions should require
;; little change for the future, except as a result of changes to address the
;; issues described in the next paragraph.

;; The current implementation is crude -- currently, the entire buffer is copied
;; on every change event. For large buffer, this comes with a significant
;; performance penalty, although for modern computers "large" means "pretty big".
;; One solution to this is offered by `linked-buffer-delayed-configuration'; this
;; performs the copying in the idle cycle and, as a side-effect, amalgamates
;; multiple changes into a single copy. As a second problem when switching
;; buffers rapidly, it can effectively break the undo mechanism -- or at least, I
;; think it is this that is causing the problem. Currently, there is no
;; workaround for this. We hope to address this in later releases with a more
;; fine-grained cloning mechanism.

;;; Code:

;; ** State

;; This section defines all of the variables that the basic state for
;; linked-buffer is stored in. We deliberately have as few of these as possible,
;; as this makes re-initializing the state during development as straight-forward
;; as possible.

;; #+BEGIN_SRC emacs-lisp

(require 'eieio)

(defvar linked-buffer-init 'linked-buffer-default-init
  "Function that initializes a linked-buffer.
This should set up `linked-buffer-config' appropriately.")

;; In future versions, this may get turned into a list so that we can have
;; multiple linked buffers, but it is not clear how the user interface
;; functions such as `linked-buffer-swap-window' would work now.
(defvar linked-buffer-config nil
  "Configuration for linked-buffer.

This is object created by function linked-buffer-configuration',
which defines the way in which the text in the different buffers
is kept synchronized. This configuration is resiliant to changes
of mode in the current buffer.")

(make-variable-buffer-local 'linked-buffer-config)
(put 'linked-buffer-config 'permanent-local t)

(defvar linked-buffer-init-functions nil
  "A list of all functions that can be used as linked-buffer-init
  functions.")

(defun linked-buffer-config-name (buffer)
  "Given BUFFER, return a name for the configuration object."
  (format "linked: %s" buffer))


;; #+end_src

;; ** Base Configuration

;; This section defines the base class and generic methods for all
;; linked-buffer-configuration objects.


;; #+begin_src emacs-lisp
;;
;; Base Configuration:
;;
(defclass linked-buffer-configuration ()
  ((this-buffer
    :initarg :this-buffer)
   (that-buffer
    :initarg :that-buffer)
   (sync-point
    :initarg :sync-point
    :initform t)
   (last-change-start-converted
    :initarg :last-change-start-converted
    :initform nil))
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

;; #+end_src

;; ** Default Configuration

;; Two buffers with exactly the same contents, like an indirect buffer but
;; without the equivalent transfer of text properties.


;; #+begin_src emacs-lisp

(defclass linked-buffer-default-configuration (linked-buffer-configuration)
  ((linked-file
    :initform nil
    :initarg :linked-file)
   (linked-mode
    :initform 'normal-mode
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
    ;; insert the contents
    (linked-buffer-update-contents conf)
    ;; init that-buffer with mode, file and config
    ;; the mode must be init'd after adding content in case there are any
    ;; file-local variables need to be evaled
    (with-current-buffer that-buffer
      (when sec-mode
        (funcall sec-mode))
      (when sec-file
        (set-visited-file-name sec-file))
      (setq linked-buffer-config
            (linked-buffer-invert conf)))
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

;; before-change-functions (beginning-of-region-b4 end-of-region-b4)
;; after-change-functions (beginning-of-region-af end-of-region-af length-of-text-before-change)


;; Addition *2
;; Before-change:(192 192)
;; Updating after-change (current:linked:rest): *linked: *scratch**,*scratch*,(192 193 0)

;; Before-change:(193 193)
;; Updating after-change (current:linked:rest): *linked: *scratch**,*scratch*,(193 194 0)

;; Deletion

;; Before-change:(192 194)
;; Updating after-change (current:linked:rest): *linked: *scratch**,*scratch*,(192 192 2)

;; Do we have enough data from the after change? I think so -- we go to the
;; point at the start of the region (whcih can surely never change?), then we
;; delete the size before (2 or 0 in this case), then we put the new text in place.

;;
;; Before-change:(192 192)
;; Updating after-change (current:linked:rest): *linked: *scratch**,*scratch*,(192 193 0)



(defmethod linked-buffer-clone ((conf linked-buffer-configuration)
                                &optional start stop length-before)
  "Updates that-buffer to reflect the contents in this-buffer.

Currently, this is just a clone all method but may use regions in future."
  (let ((this-b (oref conf :this-buffer))
        (that-b (oref conf :that-buffer)))
    (with-current-buffer this-b
      (linked-buffer-log "this-b (point,start,stop)(%s,%s,%s)" (point) start stop)
      (let* ((start (or start (point-min)))
             (stop (or stop (point-max)))
             (length-before (or length-before (buffer-size that-b)))
             ;; get the start location that we converted before the change.
             ;; linked-buffer-convert is not reliable now, because the two
             ;; buffers do not share state until we have percolated it
             (converted-start
              (or (oref conf :last-change-start-converted)
                  (point-min))))
        ;; used this, so dump it
        (oset conf :last-change-start-converted nil)
        (with-current-buffer that-b
          (delete-region (max (point-min) converted-start)
                         (min (point-max)
                              (+ length-before
                                 converted-start)))
          (linked-buffer-log
           "delete (from,to):(%s,%s)"
           (max (point-min) converted-start)
           (min (point-max)
                (+ length-before
                   converted-start)))

          (save-excursion
            (linked-buffer-log "(point,start,converted):(%s,%s,%s)"
                               (point) start converted-start)
            (goto-char converted-start)
            ;; so this insertion is happening at the wrong place in block
            ;; comment -- in fact, it's happening one too early
            (insert
             (save-restriction
               (with-current-buffer this-b
                 (widen)
                 ;; want to see where it goes
                 ;; hence the property
                 (propertize
                  (buffer-substring-no-properties
                   start stop)
                  'font-lock-face 'error))))))))))

(defun linked-buffer-default-init ()
  "Default init function.
See `linked-buffer-init' for details."
  (setq linked-buffer-config
        (linked-buffer-default-configuration
         (linked-buffer-config-name (current-buffer))
         :this-buffer (current-buffer))))

(add-to-list 'linked-buffer-init-functions
             'linked-buffer-default-init)


;; #+end_src

;; ** Basic Operation

;; Hooks into Emacs change system, some basic window management tools and so on.

;; #+begin_src emacs-lisp
(defmacro linked-buffer-when-linked (&rest body)
  "Evaluate BODY when in a linked-buffer."
  `(when (and
          linked-buffer-config
          (linked-buffer-that
           linked-buffer-config)
          (buffer-live-p
           (linked-buffer-that
            linked-buffer-config)))
     ,@body))

(defun linked-buffer-ensure-hooks ()
  "Ensures that the hooks that this mode requires are in place."
  (add-hook 'post-command-hook
            'linked-buffer-post-command-hook)
  ;; after and before-change functions are hooks (with args) even if they are
  ;; not named as such.
  (add-hook 'after-change-functions
            'linked-buffer-after-change-function)
  (add-hook 'before-change-functions
            'linked-buffer-before-change-function)
  (add-hook 'after-save-hook
            'linked-buffer-after-save-hook))

(defvar linked-buffer-log t)
(defmacro linked-buffer-log (&rest rest)
  "Log REST."
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
  "Stop linked-buffer from working due to code problem"
  (interactive)
  (setq linked-buffer-emergency t))

(defun linked-buffer-unemergency ()
  (interactive)
  (setq linked-buffer-emergency nil))

(defvar linked-buffer-saving-p nil)

(defun linked-buffer-after-save-hook ()
  (linked-buffer-when-linked
   ;; don't want to recurse!
   (when (not linked-buffer-saving-p)
     (let ((linked-buffer-saving-p t))
       (with-current-buffer
           (linked-buffer-that linked-buffer-config)
         (when (buffer-file-name)
           (save-buffer)))))))

(defun linked-buffer-post-command-hook ()
  "Update point according to config, with error handling."
  ;;(message "Entering post-command-hook")
  (unless linked-buffer-emergency
    (condition-case err
        (linked-buffer-post-command-hook-1)
      (error
       (linked-buffer-hook-fail err "post-command-hook")))))

(defun linked-buffer-post-command-hook-1 ()
  "Update point according to config."
  (progn
    (linked-buffer-when-linked
     (linked-buffer-update-point linked-buffer-config))))

(defun linked-buffer-hook-fail (err hook)
  "Give an informative message when we have to fail.
ERR is the error. HOOK is the hook type."
  (message "linked-buffer mode has failed on %s hook: %s "
           hook (error-message-string err))
  (linked-buffer-emergency)
  (with-output-to-temp-buffer "*linked-buffer-fail*"
    (princ "There has been an error in linked-buffer-mode.\n")
    (princ "The following is debugging information\n\n")
    (princ (error-message-string err)))
  (select-window (get-buffer-window "*linked-buffer-fail*")))

(defun linked-buffer-move-linked-window ()
  "Move the linked-buffer into the current window.
If the linked-buffer is currently being displayed in another
window, then the current-buffer will be moved into that window.
See also `linked-buffer-swap-buffer-windows'."
  (interactive)
  (let ((before-window-start
         (window-start (get-buffer-window)))
        (before-window-point
         (point)))
    (linked-buffer-swap-buffer-windows
     (current-buffer)
     (linked-buffer-that linked-buffer-config))
    (set-window-start
     (selected-window)
     before-window-start)
    (goto-char before-window-point)))

(defun linked-buffer-swap-linked-window ()
  "Swap the window of the buffer and linked-buffer.
If both are current displayed, swap the windows they
are displayed in, which keeping current buffer.
See also `linked-buffer-move-linked-window'."
  (interactive)
  (linked-buffer-swap-buffer-windows
   (current-buffer)
   (linked-buffer-that linked-buffer-config))
  (when (window-live-p
         (get-buffer-window
          (current-buffer)))
    (select-window
     (get-buffer-window
      (current-buffer)))))

(defun linked-buffer-swap-buffer-windows (a b)
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

(defun linked-buffer-ensure-init ()
  "Ensure that the `linked-buffer-init' has been run."
  (unless (and linked-buffer-config
               (slot-boundp
                linked-buffer-config :that-buffer)
               (buffer-live-p (linked-buffer-that
                               linked-buffer-config)))
    (funcall linked-buffer-init)))

(defun linked-buffer-init-create ()
  "Create the linked-buffer for current-buffer."
  (linked-buffer-ensure-init)
  (linked-buffer-create linked-buffer-config))

(defun linked-buffer-create-in-selected-window ()
  "Create a linked buffer and move it to the current window."
  (interactive)
  (let ((before-window-start
         (window-start (get-buffer-window)))
        (before-window-point
         (point)))
    (linked-buffer-ensure-init)
    (set-window-buffer
     (selected-window)
     (linked-buffer-create linked-buffer-config))
    (set-window-start
     (selected-window)
     before-window-start)
    (goto-char before-window-point)))

(defun linked-buffer-split-window-below ()
  "Create a linked buffer in a new window below."
  (interactive)
  (linked-buffer-ensure-init)
  (set-window-buffer
   (split-window-below)
   (linked-buffer-create linked-buffer-config)))

(defun linked-buffer-split-window-right ()
  "Create a linked buffer in a new window right."
  (interactive)
  (linked-buffer-ensure-init)
  (set-window-buffer
   (split-window-right)
   (linked-buffer-create linked-buffer-config)))

(defun linked-buffer-after-change-function (start stop length-before)
  "Run change update according to `linked-buffer-config'.
Errors are handled. REST is currently just ignored."
  (unless linked-buffer-emergency
    (condition-case err
        (linked-buffer-after-change-function-1 start stop length-before)
      (error
       (linked-buffer-hook-fail err "after change")))))

(defun linked-buffer-after-change-function-1 (start stop length-before)
  "Run change update according to `linked-buffer-config'.
REST is currently just ignored."
  (linked-buffer-when-linked
   (linked-buffer-log
    "Updating after-change (current:linked:rest): %s,%s,%s"
    (current-buffer)
    (linked-buffer-that linked-buffer-config)
    (list start stop length-before))
   (linked-buffer-update-contents linked-buffer-config
                                  start stop length-before)))


;; convert the start position and store it. we need to do this BEFORE
;; the change so that we can use the value during clone. After the
;; change, this-buffer and that-buffer will have different contents
;; (until the change has been percolated). and the convert function
;; may not work properly under these circumstances.
(defun linked-buffer-before-change-function (start stop)
  "Run before change update.
REST is currently ignored. Currently this does nothing."
  (unless linked-buffer-emergency
    (condition-case err
        (linked-buffer-when-linked
         (oset linked-buffer-config
               :last-change-start-converted
               (linked-buffer-convert
                linked-buffer-config
                start)))
        (linked-buffer-log
         "Before-change:%s" rest)
        (lambda ())
      (error
       (linked-buffer-hook-fail err "before change")))))

(defun linked-buffer-update-contents (conf &optional start stop length-before)
  "Update the contents of that-buffer with the contents of this-buffer.
Update mechanism depends on CONF."
  (unwind-protect
      (progn
        (setq inhibit-read-only t)
        (linked-buffer-log
         "Update config: %s" linked-buffer-config)
        (linked-buffer-clone conf start stop length-before))
    (setq inhibit-read-only nil)))

(defun linked-buffer-update-point (conf)
  "Update the location of point in that-buffer to reflect this-buffer.
This also attempts to update any windows so that they show the
same top-left location. Update details depend on CONF."
  ;; only sync when we are told to!
  (when (oref conf :sync-point)
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
      (linked-buffer-log "sync(front-point)(%s)" from-point)
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
       (get-buffer-window-list (linked-buffer-that conf))))))


;; #+end_src

;; ** Minor Mode

;; #+begin_src emacs-lisp
(defun linked-buffer-toggle-auto-sync-point ()
  (interactive)
  (linked-buffer-when-linked
   (oset linked-buffer-config :sync-point
         (not (oref linked-buffer-config :sync-point)))))

(defvar linked-buffer-mode-map (make-sparse-keymap)
  "Keymap for linked-buffer-minor-mode")

(define-key linked-buffer-mode-map
  (kbd "C-c ,s") 'linked-buffer-swap-linked-window)

(define-key linked-buffer-mode-map
  (kbd "C-c ,h") 'linked-buffer-move-linked-window)

(define-minor-mode linked-buffer-mode
  :lighter "lb"
  :keymap linked-buffer-mode-map)

(easy-menu-change
 '("Edit")
 "Linked"
 '(["Create Here" linked-buffer-create-in-selected-window]
   ["Split Below" linked-buffer-split-window-below]
   ["Split Right" linked-buffer-split-window-right]
   ["Move Here" linked-buffer-move-linked-window :active linked-buffer-config]
   ["Swap" linked-buffer-swap-buffer-windows :active linked-buffer-config]))

(defun linked-buffer-insert-file-local (init-function)
  (interactive
   (list (completing-read
          "Linked-Buffer init function: "
          (mapcar
           'symbol-name
           linked-buffer-init-functions)
          'identity 'confirm)))
  (save-excursion
    (goto-char (point-max))
    (let ((start (point)))
      (insert
       (format
        ;; split this string or we get local variable not terminated properly
        ;; errors.
        (concat "\nLocal"
                " Variables:\nlinked-buffer-init: %s\nEnd:\n") init-function))
      (comment-region start (point)))))

(defvar linked-buffer-start-mode-map (make-sparse-keymap))

(define-key linked-buffer-start-mode-map
  (kbd "C-c ,b") 'linked-buffer-split-window-below)

(define-key linked-buffer-start-mode-map
  (kbd "C-c ,r") 'linked-buffer-split-window-right)

(define-key linked-buffer-start-mode-map
  (kbd "C-c ,f") 'linked-buffer-insert-file-local)

(define-key linked-buffer-start-mode-map
  (kbd "C-c ,c") 'linked-buffer-create-in-selected-window)


(define-minor-mode linked-buffer-start-mode
  :lighter ""
  :keymap linked-buffer-start-mode-map)

(define-globalized-minor-mode global-linked-buffer-start-mode
  linked-buffer-start-mode
  linked-buffer-start-on)

(defun linked-buffer-start-on ()
  (linked-buffer-start-mode 1))


;; #+end_src

;; ** Test Functions

;; Functions which are used for testing new linked-buffer-configurations; as such
;; they are either batch operation functionality, or interactive commands to run
;; the various hook commands rather than from the post-command or after-change
;; hook functionality.

;; #+begin_src emacs-lisp
(defun linked-buffer-batch-clone-and-save-with-config (filename init)
  "Open FILENAME, set INIT function, then clone and save.

This function does potentially evil things if the file or the
linked-buffer is open already."
  (with-current-buffer
      (find-file-noselect filename)
    (setq linked-buffer-init init)
    (with-current-buffer
        (linked-buffer-init-create)
      (save-buffer)
      (kill-buffer))
    (kill-buffer)))

(defun linked-buffer-batch-clone-with-config
  (filename init)
  "Open FILENAME, set INIT function, then clone.

Return the linked-buffer contents without properties."
  (let ((retn nil))
    (with-current-buffer
        (find-file-noselect filename)
      (setq linked-buffer-init init)
      (with-current-buffer
          (linked-buffer-init-create)
        (setq retn
              (buffer-substring-no-properties
               (point-min)
               (point-max)))
        (set-buffer-modified-p nil)
        (kill-buffer))
      (set-buffer-modified-p nil)
      (kill-buffer))
    retn))

(defun linked-buffer-test-after-change-function ()
  "Run the change functions out of the command loop.
Using this function is the easiest way to test an new
`linked-buffer-clone' method, as doing so in the command loop is
painful for debugging. Set variable `linked-buffer-emergency' to
true to disable command loop functionality."
  (interactive)
  (linked-buffer-after-change-function-1 nil))

(defun linked-buffer-test-post-command-hook ()
  "Run the post-command functions out of the command loop.
Using this function is the easiest way to test an new
`linked-buffer-convert' method, as doing so in the command loop is
painful for debugging. Set variable `linked-buffer-emergency' to
true to disable command loop functionality."
  (interactive)
  (linked-buffer-post-command-hook-1))

(defun linked-buffer-test-reinit ()
  "Recall the init function regardless of current status.
This can help if you have change the config object and need
to make sure there is a new one."
  (interactive)
  (funcall linked-buffer-init))

(provide 'linked-buffer)

;; #+END_SRC

;;; linked-buffer.el ends here
