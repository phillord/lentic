;; linked-buffer-delayed.el --- linked-buffers but slowly -*- lexical-binding: t -*-

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
;; Currently, the implementation of things like linked-buffer-block.el is
;; quite slow. This is exacerbated by changes which percolate though large
;; parts of the buffer. Of course, the obvious solution would be to fix the
;; dodgy algorithms in linked-buffer-block, but why do that when instead you
;; can run the same dodgy algorithms on the idle cycle. This has the advantage
;; that multiple changes with no idle setp in between will be aggregated.


;;; Code:
(require 'linked-buffer)
(require 'dash)

(defvar linked-buffer-delayed-queue nil)
(defvar linked-buffer-delayed-timer nil)

(defun linked-buffer-delayed-ensure-timer ()
  (unless linked-buffer-delayed-timer
    (setq linked-buffer-delayed-timer
          (run-with-idle-timer 0.05 t 'linked-buffer-delayed-timer-function))))

(defun linked-buffer-delayed-timer-function ()
  (message "linked-buffer-delayed-timer running")
  (let ((repeat t))
    (while
        (and repeat
             linked-buffer-delayed-queue)
      (linked-buffer-delayed-clone
       (pop linked-buffer-delayed-queue))
      ;; redisplay and wait
      (setq repeat (sit-for 0.1))))
  ;; kill timer if we have finished
  (unless linked-buffer-delayed-queue
    (cancel-timer linked-buffer-delayed-timer)
    (setq linked-buffer-delayed-timer nil)))

(defclass linked-buffer-delayed-configuration
  (linked-buffer-configuration)
  ((delayed
    :initarg :delayed))
  "Linked-buffer in the idle cycle.")

(defmethod linked-buffer-this ((conf linked-buffer-delayed-configuration))
  (linked-buffer-this (oref conf :delayed)))

(defmethod linked-buffer-that ((conf linked-buffer-delayed-configuration))
  (linked-buffer-that (oref conf :delayed)))

(defmethod linked-buffer-create ((conf linked-buffer-delayed-configuration))
  (let ((buf (linked-buffer-create (oref conf :delayed))))
    ;; we should now have set up that-buffer and config, which will be the
    ;; undelayed form -- so swap a new one in
    (with-current-buffer
        (linked-buffer-that conf)
      (setq linked-buffer-config
            (linked-buffer-delayed-configuration
             "inverted-delayed"
             :delayed linked-buffer-config)))
    buf))


(defmethod linked-buffer-convert ((conf linked-buffer-delayed-configuration)
                                  location)
  (linked-buffer-convert (oref conf :delayed) location))

(defmethod linked-buffer-clone ((conf linked-buffer-delayed-configuration))
  (add-to-list 'linked-buffer-delayed-queue
               conf t)
  (linked-buffer-delayed-ensure-timer))

(defmethod linked-buffer-delayed-clone ((conf
                                         linked-buffer-delayed-configuration))
  ;; inhibit-modification-hooks or we percolate back to the start
  (let ((inhibit-modification-hooks t))
    (linked-buffer-clone (oref conf :delayed))
    (linked-buffer-update-point (oref conf :delayed))))

(defun linked-buffer-delayed-init (delayed)
  (unless linked-buffer-config
    (funcall delayed)
    (setq linked-buffer-config
          (linked-buffer-delayed-configuration "delayed" :delayed linked-buffer-config))))

(provide 'linked-buffer-delayed)
