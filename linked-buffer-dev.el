(require 'linked-buffer)

(defvar linked-buffer-dev-insert-face 'font-lock-keyword-face)

(defun linked-buffer-dev-rotate-face ()
  (interactive)
  (setq linked-buffer-dev-insert-face
        (nth (random (length (face-list)))
             (face-list)))
  (message "Insert face is now %s"
           (propertize
            "this"
            'face
            linked-buffer-dev-insert-face)))

(defadvice linked-buffer-insertion-string-transform
  (after face-transform
         (string)
         disable)
  (setq ad-return-value
        (propertize
         string
         'font-lock-face
         linked-buffer-dev-insert-face)))

(defvar linked-buffer-enable-insertion-marking nil)
(defun linked-buffer-enable-insertion-marking ()
  (interactive)
  (if linked-buffer-enable-insertion-marking
      (progn
        (ad-deactivate 'linked-buffer-insertion-string-transform)
        (setq linked-buffer-enable-insertion-marking nil)
        (message "Insertion marking off"))
    (ad-enable-advice 'linked-buffer-insertion-string-transform
                      'after 'face-transform)
    (ad-activate 'linked-buffer-insertion-string-transform)
    (setq linked-buffer-enable-insertion-marking t)
    (message "Insertion marking on")))

(provide 'linked-buffer-dev)
