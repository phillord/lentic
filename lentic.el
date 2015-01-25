;;; lentic.el --- One buffer as a view of another -*- lexical-binding: t -*-

;;; Header:

;; This file is not part of Emacs

;; Author: Phillip Lord <phillip.lord@newcastle.ac.uk>
;; Maintainer: Phillip Lord <phillip.lord@newcastle.ac.uk>
;; Version: 0.6.2
;; Package-Requires: ((emacs "24")(m-buffer "0.8")(dash "2.5.0")(f "0.17.2"))

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

;; `lentic' enables /lenticular text/: simultaneous editing and viewing of the
;; same (or closely related) text in two or more buffers, potentially in
;; different modes. Lenticular text is named after lenticular printing, which
;; produce images which change depending on the angle at which they are
;; viewed.

;; Sometimes, it would be nice to edit a file in two ways at once. For
;; instance, you might have a source file in a computational language with
;; richly marked documentation. As Emacs is a modal editor, it would be nice
;; to edit this file both in a mode for the computational language and for the
;; marked up documentation.

;; One solution to this is to use a single-mode which supports both types of
;; editing. The problem with this is that it is fundamentally difficult to
;; support two types of editing at the same time; more over, you need a new
;; mode for each combination. Another solution is to use one of the
;; multiple-mode tools which are available. The problem with this is that they
;; generally need some support from the modes in question. And, again, the
;; dificulty is supporting both forms of editing in the same environment. A
;; final problem is that it is not just the editing environment that needs to
;; be adapted; the programmatic environment needs to be untroubled by the
;; documentation, and the documentation environment untroubled by the program
;; code.

;; Lenticular text provides an alternative solution. Two lentic buffers, by
;; default, the share content but are otherwise independent. Therefore,
;; you can have two buffers open, each showing the content in different modes;
;; to switch modes, you simply switch buffers. The content, location of point,
;; and view are shared.

;; Moreover, lentic buffers can also perform a bi-directional transformation
;; between the two. If this is done, then the two can have different but
;; related text. This also solves the problem of integration with a
;; tool-chain; each lentic buffer can be associated with a different file and
;; a different syntax. For example, this file is, itself, lenticular text. It
;; can be viewed either as Emacs-Lisp or in Org-Mode. In Emacs-Lisp mode, this
;; text is commented out, in org-mode it is not.

;; In fact, although the default behaviour of lentic appears to keep the same
;; text in each buffer, even it uses this bi-directional transformation
;; capability; while the text is shared, the text properties are not. This is
;; a behaviour which differs between lentic buffers and indirect buffers. The
;; lentic buffers can therefore be in different modes without fighting each
;; other to set the text properties.

;; It is possible to configure the transformation for any two buffers in a
;; extensible way. Mostly I have concentrated on mode-specific operation,
;; but, for instance, I have also used this ability on a per-project basis
;; controlling, for instance, the location of the lentic-file.

;;; Usage:

;; lentic can be installed through MELPA/Marmalade then add

;; (global-lentic-start-mode)

;; to your .emacs.

;; The main user entry points are accessible through the lentic edit menu, or
;; through `global-lentic-start-mode' which adds keybindings to tools to create a
;; new lentic buffer. `lentic-mode-create-in-selected-window' will create a
;; lentic-buffer swap it to the current window, while
;; `lentic-mode-split-window-below' will split the current window and create a
;; lentic buffer.

;; By default, the lentic buffer created contains exactly the same contents as
;; the original buffer, but is otherwise separate; it can have a different major
;; modes, different syntax highlighting, invisible regions and even different
;; narrowing. Saving one buffer will save the other; killing the lentic buffer
;; does not affect the original, but killing the original also kills the lentic.

;; While this is somewhat useful, more generally a buffer will be configured to
;; produce a particular transformation. This can control many features of the
;; lentic, including the file name, major mode and an arbitrary transformation
;; between the two. Configuration is considered next.

;;; Configuration:

;; lentic buffers are configurable in a large number of ways. It is possible
;; to control the nature of the transformation, the default buffer name that a
;; lentic buffer takes, and the file location (or not) of the lentic buffer.
;; For this release of lentic currently, each buffer can have a single lentic
;; buffer, although this restriction will be removed in later versions.

;; Configuration of a buffer happens in one of two places. First,
;; `lentic-init' is run when a lentic buffer is first created. This function
;; should return the configuration object, and is mostly designed for use as a
;; file-local or dir-local variable. This object is stored in the `lentic-config'
;; and all subsequent operation happens through this.

;; There are now a number of different configurations, which can be used for
;; general-purposes use as well as an extension points for subclass
;; configurations. The two most general configurations are:

;;  - default: this copies all text exactly, but does not transfer
;;    text-properties (which is the behaviour of indirect buffers). It is
;;    possible to configure the default file or mode on a per-object basis.
;;  - block: this is designed for programmatic syntaxes where blocks of code are
;;    demarcated by start and end tags, and everything else is commented by
;;    line-start comments. Comments are added or removed between the two buffers.

;; The second of these is extended in lentic-org.el to provide the
;; configuration for this file: there is a normal emacs-lisp file in one buffer
;; and an org-mode version in another. Other programmatic and documentation modes
;; are supported in other files.

;;; Status:

;; This is an early release. It is generallly functional now and seems to be
;; stable, however, there is the possibility that it will behave badly and may
;; result in data loss. Please use with care on files with backups.

;; Previous releases of this package were called "linked-buffer". I changed
;; this because I wanted a name for the general idea of text with two
;; visualisations; "linked text" doesn't work because it is sounds like
;; hyperlinked text.

;; Although it is still too early to guarantee, I hope that the current
;; configuration scheme will remain fixed, and subclass extensions should
;; require little change for the future, except as a result of changes to
;; address the issues described in the next paragraph.

;; Generally, the implementation of lentic uses Emacs native change hooks and
;; transfers only the changed text between the two buffers. Some
;; configurations need to transfer slightly more text, and need to perform
;; whole buffer analysis on every keypress. This mechanism is reasonably
;; performant. On a modern machine, buffers of 4k lines can be edited without
;; noticable lag, and profiling suggests that lentic uses less than 5% of CPU
;; in normal usage. Lentic also supports a fall-back implementation which
;; copies the whole buffer after every keypress; this is much easier to
;; write new configurations for, and is still reasonable performant to 3-400
;; line buffers.


;;; Code:

;; ** State

;; This section defines all of the variables that the basic state for lentic
;; is stored in. We deliberately have as few of these as possible, as this
;; makes re-initializing the state during development as straight-forward as
;; possible.

;; #+BEGIN_SRC emacs-lisp

(require 'eieio)
(require 'm-buffer)

(defvar lentic-init 'lentic-default-init
  "Function that initializes a lentic.
This should set up `lentic-config' appropriately.")

(make-variable-buffer-local 'lentic-init)

;; In future versions, this may get turned into a list so that we can have
;; multiple lentic buffers, but it is not clear how the user interface
;; functions such as `lentic-swap-window' would work now.
(defvar lentic-config nil
  "Configuration for lentic.

This is object created by function lentic-configuration',
which defines the way in which the text in the different buffers
is kept synchronized. This configuration is resiliant to changes
of mode in the current buffer.")

(make-variable-buffer-local 'lentic-config)
(put 'lentic-config 'permanent-local t)

(defvar lentic-init-functions nil
  "A list of all functions that can be used as lentic-init
  functions.")

(defun lentic-config-name (buffer)
  "Given BUFFER, return a name for the configuration object."
  (format "lentic: %s" buffer))
;; #+end_src

;; ** Base Configuration

;; This section defines the base class and generic methods for all
;; lentic-configuration objects.


;; #+begin_src emacs-lisp
;;
;; Base Configuration:
;;
(defclass lentic-configuration ()
  ((this-buffer
    :initarg :this-buffer)
   (that-buffer
    :initarg :that-buffer)
   (creator
    :initarg :creator
    :initform nil
    :documentation
    "Non-nil if this lentic-configuration was used to create a lentic view.")
   (delete-on-exit
    :initarg :delete-on-exit
    :initform nil
    :documentation
    "Non-nil if the file associated with this should be deleted on exit")
   (sync-point
    :initarg :sync-point
    :initform t)
   (last-change-start
    :initarg :last-change-start
    :initform nil)
   (last-change-start-converted
    :initarg :last-change-start-converted
    :initform nil)
   (last-change-stop
    :initarg :last-change-stop
    :initform nil)
   (last-change-stop-converted
    :initarg :last-change-stop-converted
    :initform nil))
  "Configuration object for lentic, which defines the mechanism
by which linking happens.")

(defgeneric lentic-create (conf))
(defgeneric lentic-convert (conf location))
(defgeneric lentic-invert (conf that-buffer))

(defmethod lentic-this ((conf lentic-configuration))
  (oref conf :this-buffer))

(defmethod lentic-that ((conf lentic-configuration))
  (oref conf :that-buffer))

(defmethod lentic-ensure-that ((conf lentic-configuration))
  "Get the lentic for this configuration
or create it if it does not exist."
  (or (lentic-that conf)
      (lentic-create conf)))

;; #+end_src

;; ** Default Configuration

;; Two buffers with exactly the same contents, like an indirect buffer but
;; without the equivalent transfer of text properties.


;; #+begin_src emacs-lisp

(defclass lentic-default-configuration (lentic-configuration)
  ((lentic-file
    :initform nil
    :initarg :lentic-file)
   (lentic-mode
    :initform 'normal-mode
    :initarg :lentic-mode))
  "Configuration which maintains two lentics with the same contents.")

(defun lentic-insertion-string-transform (string)
  "Transform the string that is about to be inserted.
This function is not meant to do anything. It's useful to
advice."
  string)

(defmethod lentic-create ((conf lentic-default-configuration))
  "Create the lentic for this configuration.
Given a `lentic-configuration' object, create the lentic
appropriate for that configurationuration. It is the callers
responsibility to check that buffer has not already been
created."
  ;; make sure the world is ready for lentic buffers
  (lentic-ensure-hooks)
  ;; create lentic
  (let* ((this-buffer
          (lentic-this conf))
         (that-buffer
          (get-buffer-create
           (format "*lentic: %s*"
                   (buffer-name
                    this-buffer))))
         (sec-mode (oref conf :lentic-mode))
         (sec-file (oref conf :lentic-file)))
    (oset conf :creator t)
    ;; make sure this-buffer knows about that-buffer
    (oset conf :that-buffer that-buffer)
    ;; insert the contents
    (lentic-update-contents conf)
    ;; init that-buffer with mode, file and config
    ;; the mode must be init'd after adding content in case there are any
    ;; file-local variables need to be evaled
    (with-current-buffer that-buffer
      (when sec-mode
        (funcall sec-mode))
      (when sec-file
        (set-visited-file-name sec-file))
      (setq lentic-config
            (lentic-invert conf)))
    that-buffer))

(defmethod lentic-invert ((conf lentic-default-configuration))
  (lentic-default-configuration
   (lentic-config-name (lentic-that conf))
   :this-buffer (lentic-that conf)
   :that-buffer (lentic-this conf)))

(defmethod lentic-convert ((conf lentic-default-configuration)
                                  location)
  "For this configuration, convert location to an equivalent location in
the lentic."
  location)

(defmethod lentic-clone ((conf lentic-configuration)
                                &optional start stop _length-before
                                start-converted stop-converted)
  "Updates that-buffer to reflect the contents in this-buffer.

Currently, this is just a clone all method but may use regions in future."
  (let ((this-b (lentic-this conf))
        (that-b (lentic-that conf)))
    (with-current-buffer this-b
      ;;(lentic-log "this-b (point,start,stop)(%s,%s,%s)" (point) start stop)
      (save-restriction
        (widen)
        (let* ((start (or start (point-min)))
               (stop (or stop (point-max))))
          (with-current-buffer that-b
            (save-restriction
              ;; get the start location that we converted before the change.
              ;; lentic-convert is not reliable now, because the two
              ;; buffers do not share state until we have percolated it
              (let ((converted-start
                     (or start-converted
                         (point-min)))
                    (converted-stop
                     (or stop-converted
                         (point-max))))
                (widen)
                (delete-region (max (point-min) converted-start)
                               (min (point-max) converted-stop))
                (save-excursion
                  (goto-char converted-start)
                  ;; so this insertion is happening at the wrong place in block
                  ;; comment -- in fact, it's happening one too early
                  (insert
                   (with-current-buffer this-b
                     ;; want to see where it goes
                     ;; hence the property
                     (lentic-insertion-string-transform
                      (buffer-substring-no-properties
                       start stop)))))))))))))

;;;###autoload
(defun lentic-default-init ()
  "Default init function.
see `lentic-init' for details."
  (lentic-default-configuration
   (lentic-config-name (current-buffer))
   :this-buffer (current-buffer)))

(add-to-list 'lentic-init-functions
             'lentic-default-init)


;; #+end_src

;; ** Basic Operation

;; Hooks into Emacs change system, some basic window management tools and so on.

;; #+begin_src emacs-lisp
(defmacro lentic-when-lentic (&rest body)
  "Evaluate BODY when in a lentic."
  (declare (debug t))
  `(when (and
          lentic-config
          (lentic-that
           lentic-config)
          (buffer-live-p
           (lentic-that
            lentic-config)))
     ,@body))

(defun lentic-ensure-hooks ()
  "Ensures that the hooks that this mode requires are in place."
  (add-hook 'post-command-hook
            'lentic-post-command-hook)
  ;; after and before-change functions are hooks (with args) even if they are
  ;; not named as such.
  (add-hook 'after-change-functions
            'lentic-after-change-function)
  (add-hook 'before-change-functions
            'lentic-before-change-function)
  (add-hook 'after-save-hook
            'lentic-after-save-hook)
  (add-hook 'kill-buffer-hook
            'lentic-kill-buffer-hook)
  (add-hook 'kill-emacs-hook
            'lentic-kill-emacs-hook))

(defvar lentic-log t)
(defmacro lentic-log (&rest rest)
  "Log REST."
  `(when lentic-log
     (lentic-when-lentic
      (let ((msg
             (concat
              (format ,@rest)
              "\n")))
        (with-current-buffer
            (get-buffer-create "*lentic-log*")
          (goto-char (point-max))
          (insert msg))))))

(defvar lentic-emergency  nil
  "Iff lentic-emergency is non-nil stop all change related
  activity.

This is not the same as disabling lentic mode. It stops
all lentic related activity in all buffers; normally this
happens as a result of an error condition. If lentic was
to carry on in these circumstances, serious data loss could
occur. In normal use, this variable will only be set as a result
of a problem with the code; it is not recoverable from a user
perspective.

It is useful to toggle this state on during development. Once
enabled, buffers will not update automaticaly but only when
explicitly told to. This is much easier than try to debug errors
happening on the after-change-hooks. The
`lentic-emergency' and `lentic-unemergency' functions
enable this.")

(defvar lentic-emergency-debug nil
  "Iff non-nil, lentic will store change data, even
during a `lentic-emergency'.

Normally, `lentic-emergency' disables all activity, but this makes
testing incremental changes charge. With this variable set, lentic will
attempt to store enough change data to operate manually. This does require
running some lentic code (notably `lentic-convert'). This is low
risk code, but may still be buggy, and so setting this variable can cause
repeated errors.")

(defun lentic-emergency ()
  "Stop lentic from working due to code problem."
  (interactive)
  (setq lentic-emergency t))

(defun lentic-unemergency ()
  "Start lentic working after stop due to code problem."
  (interactive)
  (setq lentic-emergency nil))

(defvar lentic-saving-p nil)

(defun lentic-after-save-hook ()
  (lentic-when-lentic
   ;; don't want to recurse!
   (when (not lentic-saving-p)
     (let ((lentic-saving-p t))
       (with-current-buffer
           (lentic-that lentic-config)
         (when (buffer-file-name)
           (save-buffer)))))))

(defvar lentic-kill-retain nil
  "If non-nil retain files even if requested to delete on exit.")

(defun lentic-kill-buffer-hook ()
  (lentic-when-lentic
    (progn
      ;; remove files
      (when
          (and (oref lentic-config :delete-on-exit)
               ;; might not exist if we not saved yet!
               (file-exists-p buffer-file-name)
               ;; if we are cloning in batch, we really do not want to kill
               ;; everything at the end
               (not noninteractive)
               ;; or we have blocked this anyway
               (not lentic-kill-retain))
        (delete-file (buffer-file-name)))
      ;; kill lentic-buffers
      (when (oref lentic-config :creator)
        (kill-buffer
         (lentic-that lentic-config))))))

(defun lentic-kill-emacs-hook ()
  (-map
   (lambda (b)
     (with-current-buffer
         b
       (when
           (and lentic-config
                (oref lentic-config :delete-on-exit)
                (file-exists-p buffer-file-name)
                (not noninteractive))
         (delete-file (buffer-file-name)))))
   (buffer-list)))

(defun lentic-post-command-hook ()
  "Update point according to config, with error handling."
  ;;(message "Entering post-command-hook")
  (unless lentic-emergency
    (condition-case err
        (lentic-post-command-hook-1)
      (error
       (lentic-hook-fail err "post-command-hook")))))

(defun lentic-post-command-hook-1 ()
  "Update point according to config."
  (progn
    (lentic-when-lentic
     (lentic-update-point lentic-config))))

(defun lentic-hook-fail (err hook)
  "Give an informative message when we have to fail.
ERR is the error. HOOK is the hook type."
  (message "lentic mode has failed on %s hook: %s "
           hook (error-message-string err))
  (lentic-emergency)
  (with-output-to-temp-buffer "*lentic-fail*"
    (princ "There has been an error in lentic-mode.\n")
    (princ "The following is debugging information\n\n")
    (princ (error-message-string err)))
  (select-window (get-buffer-window "*lentic-fail*")))

(defun lentic-ensure-init ()
  "Ensure that the `lentic-init' has been run."
  (unless (and lentic-config
               (slot-boundp
                lentic-config :that-buffer)
               (buffer-live-p (lentic-that
                               lentic-config)))
    (setq lentic-config
          (funcall lentic-init))))

(defun lentic-init-create ()
  "Create the lentic for current-buffer."
  (lentic-ensure-init)
  (lentic-create lentic-config))


(defvar lentic-emergency-last-change nil)
(make-variable-buffer-local 'lentic-emergency-last-change)

(defun lentic-after-change-function (start stop length-before)
  "Run change update according to `lentic-config'.
Errors are handled. REST is currently just ignored."
  ;; store values in case we want to use them
  (when lentic-emergency-debug
    (setq lentic-emergency-last-change (list start stop length-before)))
  (unless lentic-emergency
    (condition-case err
        (lentic-after-change-function-1 start stop length-before)
      (error
       (lentic-hook-fail err "after change")))))

(defun lentic-after-change-function-1 (start stop length-before)
  "run change update according to `lentic-config'.
rest is currently just ignored."
  (lentic-when-lentic
   (lentic-log
    "after-change (start, stop, length-before): %s,%s,%s"
    start stop length-before)
   (lentic-update-contents lentic-config
                           start stop length-before)))


;; convert the start position and store it. we need to do this before
;; the change so that we can use the value during clone. after the
;; change, this-buffer and that-buffer will have different contents
;; (until the change has been percolated). and the convert function
;; may not work properly under these circumstances.
(defun lentic-before-change-function (start stop)
  "run before change update."
  (unless (and
           lentic-emergency
           (not lentic-emergency-debug))
    (condition-case err
        (progn
          (lentic-when-lentic
           (oset lentic-config :last-change-start start)
           (oset lentic-config
                 :last-change-start-converted
                 (lentic-convert
                  lentic-config
                  start))
           (oset lentic-config :last-change-stop stop)
           (oset lentic-config
                 :last-change-stop-converted
                 (lentic-convert
                  lentic-config
                  stop)))
          (lentic-log
           "before change:(start,stop,start-c,stop-c,command): %s,%s,%s,%s,%s"
           start stop
           (oref lentic-config
                 :last-change-start-converted)
           (oref lentic-config
                 :last-change-stop-converted)
           this-command))
      (error
       (lentic-hook-fail err "before change")))))

(defun lentic-update-contents (conf &optional start stop length-before)
  "update the contents of that-buffer with the contents of this-buffer.
update mechanism depends on conf."
  (let ((inhibit-read-only t))
    ;; unfortunately b-c-f and a-c-f are not always consistent with each
    ;; other. b-c-f signals the maximal extent that may be changed, while
    ;; a-c-f signals the exact extend. We did our conversion on b-c-f when the
    ;; buffers were in sync, so we these are the only values we have.

    ;; Overestimate give inconsistency between the length before on a-c-f
    ;; (which is the actual) and the different between b-c-f start and stop.
    ;; Unfortunately, this can also occur in some correct circumstances --
    ;; replace-match for example can both insert and change simultaneously.
    ;; Currently, the best solution I have for this is to fall-back to a full
    ;; clone.
    (let ((skewed
           (when (and
                  ;; we can't be skewed where we have no region!
                  start stop length-before
                  ;; skews only occur in insertions which result in a positive
                  ;; length-before. This also picks up no-insertion changes
                  (and (< 0 length-before)
                       ;; = start stop means we have a deletion because
                       ;; there is no range after. Deletions seem to be
                       ;; safe.
                       (not (= start stop))))
             (lentic-log "Skew detected: %s" this-command)
             t)))
      (m-buffer-with-markers
          ((start-converted
            (when
                (and (not skewed)
                     (oref conf :last-change-start-converted))
              (set-marker (make-marker)
                          (oref conf :last-change-start-converted)
                          (lentic-that conf))))
           (stop-converted
            (when
                (and (not skewed)
                     (oref conf :last-change-stop-converted))
              (set-marker (make-marker)
                          (oref conf :last-change-stop-converted)
                          (lentic-that conf)))))
        ;; used these, so dump them
        (oset conf :last-change-start nil)
        (oset conf :last-change-start-converted nil)
        (oset conf :last-change-stop nil)
        (oset conf :last-change-stop-converted nil)
        ;;(lentic-log
        ;;"Update config: %s" lentic-config)
        (if skewed
            (lentic-clone conf)
          (lentic-clone conf start stop length-before
                        start-converted stop-converted))))))

(defun lentic-update-point (conf)
  "Update the location of point in that-buffer to reflect this-buffer.
This also attempts to update any windows so that they show the
same top-left location. Update details depend on CONF."
  ;; only sync when we are told to!
  (when (oref conf :sync-point)
    (let* ((from-point
            (lentic-convert
             conf
             (with-current-buffer
                 (lentic-this conf)
               (point))))
           (from-window-start
            (lentic-convert
             conf
             (window-start
              (get-buffer-window
               (lentic-this conf))))))
      ;; clone point in buffer important when the buffer is NOT visible in a
      ;; window at all
      ;;(lentic-log "sync(front-point)(%s)" from-point)
      (with-current-buffer
          (lentic-that conf)
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
       (get-buffer-window-list (lentic-that conf))))))
;; #+end_src


;; ** Batch Functions

;; These functions are for batch operation on lentic buffers. Mostly, these
;; are useful for writing tests, but they can be useful for generating
;; the lentic form of a file during any automated pipeline.

;; #+begin_src emacs-lisp
(defun lentic-batch-clone-and-save-with-config (filename init)
  "Open FILENAME, set INIT function, then clone and save.

This function does potentially evil things if the file or the
lentic is open already."
  (with-current-buffer
      (find-file-noselect filename)
    (setq lentic-init init)
    (with-current-buffer
        (lentic-init-create)
      (save-buffer)
      (kill-buffer))
    (kill-buffer)))

(defun lentic-batch-clone-with-config
  (filename init)
  "Open FILENAME, set INIT function, then clone.

Return the lentic contents without properties."
  (let ((retn nil))
    (with-current-buffer
        (find-file-noselect filename)
      (setq lentic-init init)
      (with-current-buffer
          (lentic-init-create)
        (setq retn
              (buffer-substring-no-properties
               (point-min)
               (point-max)))
        (set-buffer-modified-p nil)
        (kill-buffer))
      (set-buffer-modified-p nil)
      (kill-buffer))
    retn))

(provide 'lentic)

;;; lentic.el ends here
;; #+END_SRC
