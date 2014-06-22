(require 'linked-buffer)
(require 'linked-buffer-latex-code)
(require 'linked-buffer-asciidoc)
(require 'f)


(defvar linked-buffer-test-dir
  (concat
   (file-name-directory
    (find-lisp-object-file-name 'linked-buffer-init 'defvar))
   "test/"))

(defun linked-buffer-test-file (filename)
  (let ((file
         (concat linked-buffer-test-dir filename)))
    (when (not (file-exists-p file))
      (error "Test File does not exist: %s" file))
    file))

(defvar conf-default
  (linked-buffer-default-configuration "bob"))

(ert-deftest linked-buffer-conf ()
  (should
   (equal 'normal-mode
          (oref conf-default :linked-mode))))

(ert-deftest linked-buffer-simple ()
  (should
   (equal "simple\n"
          (linked-buffer-batch-clone-with-config
           (linked-buffer-test-file "simple-contents.txt")
           'linked-buffer-default-init))))

(ert-deftest linked-buffer-clojure-latex ()
  (should
   (equal
    (f-read
     (linked-buffer-test-file
      "block-comment-out.tex"))
    (linked-buffer-batch-clone-with-config
     (linked-buffer-test-file "block-comment.clj")
     'linked-buffer-clojure-latex-init))))

(ert-deftest linked-buffer-asciidoc-clojure ()
  (should
   (equal
    (f-read
     (linked-buffer-test-file "asciidoc-clj-out.clj"))
    (linked-buffer-batch-clone-with-config
     (linked-buffer-test-file "asciidoc-clj.txt")
     'linked-buffer-asciidoc-clojure-init))))
