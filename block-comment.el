;; -*- lexical-binding: t -*-

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


;; If FROM-FN is a function, it is called with two arguments, BEGIN
;; and END, which specify the part of the buffer it should convert.
;; It should convert the text by editing it in place.  Since this can
;; change the length of the text, FROM-FN should return the modified
;; end position.
;;
;; One responsibility of FROM-FN is to make sure that the beginning
;; of the file no longer matches REGEXP.  Otherwise it is likely to
;; get called again.
(require 'dash)
(require 'm-buffer)

(defun block-comment-line-to-block-region (begin end buffer)
  (m-buffer-replace-matches
   (m-buffer-matches-data
    buffer
    "^;;" begin end) ""))

(defun block-comment-line-to-block (begin end buffer)
  (-map
   (lambda (pairs)
     (block-comment-line-to-block-region
      (car pairs) (cdr pairs) buffer))
   (block-comment-marker-boundaries begin end buffer)))

(defun block-comment-block-to-line-region (begin end buffer)
  (m-buffer-replace-matches
   (m-buffer-matches-data
    buffer
    ;; should make this ignore lines beginning with ;; already
    "\\(^\\).+"
    begin end)
   ";;" 1))

(defun block-comment-block-to-line (begin end buffer)
  (-map
   ;; comment each of these regions
   (lambda (pairs)
     (block-comment-block-to-line-region
      (car pairs) (cdr pairs) buffer))
   (block-comment-marker-boundaries begin end buffer)))

(defun block-comment-marker-boundaries (begin end buffer)
  (-zip
   ;; \end{code}, i.e where we start comments
   ;; plus the start of the region
   (cons
    (set-marker (make-marker) begin buffer)
    (m-buffer-matches-beginning buffer "^;?;?\\\\end{code}"))
   ;; \begin{code}, i.e. where we stop comments
   ;; plus the end of the buffer
   (append
    (m-buffer-matches-end buffer "^;?;?\\\\begin{code}")
    (list (set-marker (make-marker) end buffer)))))


(defun block-comment-clone-contents-with-comments (from to)
  (linked-buffer-clone-contents from to)
  ;; remove the line comments in the to buffer
  (block-comment-line-to-block (point-min) (point-max) to))

(defun block-comment-clone-contents-without-comments (from to)
  (linked-buffer-clone-contents from to)
  (block-comment-block-to-line (point-min) (point-max) to))

(defun block-comment-clone (secondaryp from to)
  (if secondaryp
      (block-comment-clone-contents-without-comments from to)
    (block-comment-clone-contents-with-comments from to)))

(defun block-comment-clone-point (secondaryp from to)
  (block-comment-clone-point-nearly from to))

(defun block-comment-convert-location (location from to)
  (let ((line-plus
         (with-current-buffer from
           (list
            (line-number-at-pos location)
            (- (line-end-position)
                location)))))
    (with-current-buffer
        to
      (save-excursion
        (goto-line (car line-plus))
        (max (line-beginning-position)
             (- (line-end-position)
                (cadr line-plus)))))))



(provide 'block-comment)

(defun test ()
  (interactive)
  (message "locn: %s"
           (block-comment-convert-location
            (point)
            (current-buffer)
            (get-buffer "*bob*"))))
