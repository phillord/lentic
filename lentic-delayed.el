;; lentic-delayed.el --- lentics but slowly -*- lexical-binding: t -*-

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
;;
;; Currently, the implementation of things like lentic-block.el is
;; quite slow. This is exacerbated by changes which percolate though large
;; parts of the buffer. Of course, the obvious solution would be to fix the
;; dodgy algorithms in lentic-block, but why do that when instead you
;; can run the same dodgy algorithms on the idle cycle. This has the advantage
;; that multiple changes with no idle setp in between will be aggregated.


;;; Code:
(require 'lentic)
(require 'dash)

(defvar lentic-delayed-queue nil)
(defvar lentic-delayed-timer nil)

(defun lentic-delayed-ensure-timer ()
  (unless lentic-delayed-timer
    (setq lentic-delayed-timer
          (run-with-idle-timer 0.05 t 'lentic-delayed-timer-function))))

(defun lentic-delayed-timer-function ()
  (message "lentic-delayed-timer running")
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
    (cancel-timer lentic-delayed-timer)
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

(defmethod lentic-clone ((conf lentic-delayed-configuration))
  (add-to-list 'lentic-delayed-queue
               conf t)
  (lentic-delayed-ensure-timer))

(defmethod lentic-delayed-clone ((conf
                                         lentic-delayed-configuration))
  ;; inhibit-modification-hooks or we percolate back to the start
  (let ((inhibit-modification-hooks t))
    (lentic-clone (oref conf :delayed))
    (lentic-update-point (oref conf :delayed))))

(defun lentic-delayed-init (delayed)
  (unless lentic-config
    (funcall delayed)
    (setq lentic-config
          (lentic-delayed-configuration "delayed" :delayed lentic-config))))

(provide 'lentic-delayed)
