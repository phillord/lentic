(require 'linked-buffer)


(ert-deftest function-for-modes ()
  (should
   (eq
    'block-comment-clone-contents-with-comments
    (linked-buffer-extract-contents
     '(clojure-mode latex-mode))))

  (should
   (eq
    'block-comment-convert-location
    (linked-buffer-extract-point
     '(clojure-mode latex-mode)))))
