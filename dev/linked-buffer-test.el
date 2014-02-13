(require 'linked-buffer)


(ert-deftest function-for-modes ()
  (should
   (eq
    'block-comment-clone-contents-with-comments
    (linked-buffer-content-function-for-modes
     '(clojure-mode latex-mode))))

  (should
   (eq
    'block-comment-convert-location
    (linked-buffer-convert-location-for-modes
     '(clojure-mode latex-mode)))))
