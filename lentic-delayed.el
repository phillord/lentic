;; lentic-delayed.el --- lentics but slowly -*- lexical-binding: t -*-

;;; Header:

;; This file is not part of Emacs

;; Author: Phillip Lord <phillip.lord@newcastle.ac.uk>
;; Maintainer: Phillip Lord <phillip.lord@newcastle.ac.uk>
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

;; This runs lentic updates in the idle cycle. It is not really necessary now,
;; as lentic now percolates updates incrementally, rather than copying entire
;; buffers with every keypress.

;; However, incremental updates are a lot harder to implement than cloning the
;; entire buffer, so this might still be useful.

;;; Code:
;; #+begin_src emacs-lisp
(require 'lentic)
(require 'dash)

(defvar lentic-delayed-queue nil)
(defvar lentic-delayed-timer nil)

(defun lentic-delayed-ensure-timer ()
  (unless lentic-delayed-timer
    (setq lentic-delayed-timer
          (run-with-idle-timer 0.2 t 'lentic-delayed-timer-function))))

(defun lentic-delayed-timer-function ()
  (let ((repeat t))
    (while
        (and repeat
             lentic-delayed-queue)
      (lentic-delayed-clone
       (pop lentic-delayed-queue))
      ;; redisplay and wait
      (setq repeat (sit-for 0.1))))
  ;; kill timer if we have finished
  (unless lentic-delayed-queue
    (when lentic-delayed-timer
      (cancel-timer lentic-delayed-timer))
    (setq lentic-delayed-timer nil)))

(defclass lentic-delayed-configuration
  (lentic-configuration)
  ((delayed
    :initarg :delayed))
  "lentic in the idle cycle.")

(defmethod lentic-this ((conf lentic-delayed-configuration))
  (lentic-this (oref conf :delayed)))

(defmethod lentic-that ((conf lentic-delayed-configuration))
  (lentic-that (oref conf :delayed)))

(defmethod lentic-create ((conf lentic-delayed-configuration))
  (let ((buf (lentic-create (oref conf :delayed))))
    ;; we should now have set up that-buffer and config, which will be the
    ;; undelayed form -- so swap a new one in
    (with-current-buffer
        (lentic-that conf)
      (setq lentic-config
            (lentic-delayed-configuration
             "inverted-delayed"
             :delayed lentic-config)))
    buf))


(defmethod lentic-convert ((conf lentic-delayed-configuration)
                           location)
  (lentic-convert (oref conf :delayed) location))

(defmethod lentic-clone ((conf lentic-delayed-configuration)
                         &rest _rest)
  (add-to-list 'lentic-delayed-queue
               conf t)
  (lentic-delayed-ensure-timer))

(defmethod lentic-delayed-clone ((conf
                                  lentic-delayed-configuration))
  ;; inhibit-modification-hooks or we percolate back to the start
  (let ((inhibit-modification-hooks t))
    (lentic-clone (oref conf :delayed))
    (lentic-update-point (oref conf :delayed))))

;;;###autoload
(defun lentic-delayed-init (delayed)
  (unless lentic-config
    (lentic-delayed-configuration
     "delayed"
     :delayed (funcall delayed))))

;;;###autoload
(defun lentic-delayed-default-init ()
  (lentic-delayed-init
   'lentic-default-init))

(provide 'lentic-delayed)
;;; lentic-delayed.el ends here
;; #+end_src
