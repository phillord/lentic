(require 'linked-buffer)

(defvar linked-buffer-dev-insert-faces
  '(font-lock-builtin-face
    font-lock-comment-face
    font-lock-constant-face
    font-lock-doc-face
    font-lock-function-name-face
    font-lock-keyword-face
    font-lock-negation-char-face
    font-lock-preprocessor-face))

(defun linked-buffer-dev-rotate-face ()
  (interactive)
  (setq linked-buffer-dev-insert-faces
        (-rotate 1 linked-buffer-dev-insert-faces))
  (message "Insert face is now %s"
           (propertize
            "this"
            'face
            (car linked-buffer-dev-insert-faces))))

(defadvice linked-buffer-insertion-string-transform
  (after face-transform
         (string)
         disable)
  (setq ad-return-value
        (propertize
         string
         'font-lock-face
         (car linked-buffer-dev-insert-faces))))

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
