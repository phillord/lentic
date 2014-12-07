(require 'lentic)

(defvar lentic-dev-insert-face 'font-lock-keyword-face)

(defun lentic-dev-rotate-face ()
  (interactive)
  (setq lentic-dev-insert-face
        (nth (random (length (face-list)))
             (face-list)))
  (message "Insert face is now %s"
           (propertize
            "this"
            'face
            lentic-dev-insert-face)))

(defadvice lentic-insertion-string-transform
  (after face-transform
         (string)
         disable)
  (setq ad-return-value
        (propertize
         string
         'font-lock-face
         lentic-dev-insert-face
         'face
         lentic-dev-insert-face)))

(defvar lentic-enable-insertion-marking nil)
(defun lentic-enable-insertion-marking ()
  (interactive)
  (if lentic-enable-insertion-marking
      (progn
        (ad-deactivate 'lentic-insertion-string-transform)
        (setq lentic-enable-insertion-marking nil)
        (message "Insertion marking off"))
    (ad-enable-advice 'lentic-insertion-string-transform
                      'after 'face-transform)
    (ad-activate 'lentic-insertion-string-transform)
    (setq lentic-enable-insertion-marking t)
    (message "Insertion marking on")))

(provide 'lentic-dev)
