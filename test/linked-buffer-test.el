(require 'linked-buffer)
(require 'linked-buffer-latex-code)
(require 'linked-buffer-asciidoc)
(require 'linked-buffer-org)
(require 'f)


(defvar linked-buffer-test-dir
  (concat
   (file-name-directory
    (find-lisp-object-file-name 'linked-buffer-init 'defvar))
   "dev-resources/"))

(defun linked-buffer-test-file (filename)
  (let ((file
         (concat linked-buffer-test-dir filename)))
    (when (not (file-exists-p file))
      (error "Test File does not exist: %s" file))
    file))

(defun linked-buffer-test-clone-equal (init file cloned-file)
  (equal
   (f-read
    (linked-buffer-test-file cloned-file))
   (linked-buffer-batch-clone-with-config
    (linked-buffer-test-file file) init)))

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
   (linked-buffer-test-clone-equal
    'linked-buffer-clojure-latex-init
    "block-comment.clj" "block-comment-out.tex")))


;; (ert-deftest linked-buffer-asciidoc-clojure ()
;;   (should
;;    (linked-buffer-test-clone-equal
;;     'linked-buffer-asciidoc-clojure-init
;;     "asciidoc-clj.txt" "asciidoc-clj-out.clj")))


;; ;; org mode start up prints out "OVERVIEW" from the cycle. Can't see any way
;; ;; to stop this
;; (ert-deftest linked-buffer-org-el ()
;;   (should
;;    (linked-buffer-test-clone-equal
;;     'linked-buffer-org-el-init
;;     "org-el.org" "org-el.el")))

;; (ert-deftest linked-buffer-el-org ()
;;   (should
;;    (linked-buffer-test-clone-equal
;;     'linked-buffer-el-org-init
;;     "el-org.el" "el-org.org")))

;; (ert-deftest linked-buffer-orgel-org()
;;   (should
;;    (linked-buffer-test-clone-equal
;;     'linked-buffer-orgel-org-init
;;     "orgel-org.el" "orgel-org.org")))

;; (ert-deftest linked-buffer-org-orgel()
;;   (should
;;    (linked-buffer-test-clone-equal
;;     'linked-buffer-org-orgel-init
;;     "org-orgel.org" "org-orgel.el")))


;; (ert-deftest linked-buffer-org-clojure ()
;;   (should
;;    (linked-buffer-test-clone-equal
;;     'linked-buffer-org-clojure-init
;;     "org-clojure.org" "org-clojure.clj"
;;     )))

;; Use this to generate a new test file

;; (linked-buffer-batch-clone-and-save-with-config
;;    "../dev-resources/org-clojure.org"
;;    'linked-buffer-org-clojure-init)
