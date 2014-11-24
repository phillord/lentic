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

(defun linked-buffer-test-equal-loudly (a b)
  "Actually, this just tests equality and shouts if not."
  ;; change this to t to disable noisy printout
  (if t
      (string= a b)
    (if (string= a b)
        t
      (message "Results:\n%s\n:Complete\nShouldbe:\n%s\nComplete:" cloned-results cloned-file)
      (let* ((a-buffer
              (generate-new-buffer "a"))
             (b-buffer
              (generate-new-buffer "b"))
             (a-file
              (make-temp-file
               (buffer-name a-buffer)))
             (b-file
              (make-temp-file
               (buffer-name b-buffer))))
        (with-current-buffer
            a-buffer
          (insert a)
          (write-file a-file))
        (with-current-buffer
            b-buffer
          (insert b)
          (write-file b-file))
        (message "diff:%senddiff:"
                 (with-temp-buffer
                   (call-process
                    "diff"
                    nil
                    (current-buffer)
                    nil
                    "-c"
                    a-file
                    b-file)
                   (buffer-string))))
      nil)))

(defun linked-buffer-test-clone-equal (init file cloned-file)
  (let ((cloned-file
         (f-read
          (linked-buffer-test-file cloned-file)))
        (cloned-results
         (linked-buffer-batch-clone-with-config
          (linked-buffer-test-file file) init)))
    (linked-buffer-test-equal-loudly cloned-file cloned-results)))

(defun linked-buffer-test-clone-equal-generate
  (init file cloned-file)
  "Generates the test file for `linked-buffer-batch-clone-equal'."
  (f-write
   (linked-buffer-batch-clone-with-config
    (linked-buffer-test-file file) init)
   'utf-8
   (concat linked-buffer-test-dir cloned-file))
  ;; return nil, so if we use this in a test by mistake, it will crash out.
  nil)

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


(ert-deftest linked-buffer-asciidoc-clojure ()
  (should
   (linked-buffer-test-clone-equal
    'linked-buffer-asciidoc-clojure-init
    "asciidoc-clj.txt" "asciidoc-clj-out.clj")))

;; org mode start up prints out "OVERVIEW" from the cycle. Can't see any way
;; to stop this
(ert-deftest linked-buffer-org-el ()
  (should
   (linked-buffer-test-clone-equal
    'linked-buffer-org-el-init
    "org-el.org" "org-el.el")))

(ert-deftest linked-buffer-el-org ()
  (should
   (linked-buffer-test-clone-equal
    'linked-buffer-el-org-init
    "el-org.el" "el-org.org")))

(ert-deftest linked-buffer-orgel-org()
  (should
   (linked-buffer-test-clone-equal
    'linked-buffer-orgel-org-init
    "orgel-org.el" "orgel-org.org")))

(ert-deftest linked-buffer-org-orgel()
  (should
   (linked-buffer-test-clone-equal
    'linked-buffer-org-orgel-init
    "org-orgel.org" "org-orgel.el")))


(ert-deftest linked-buffer-org-clojure ()
  (should
   (linked-buffer-test-clone-equal
    'linked-buffer-org-clojure-init
    "org-clojure.org" "org-clojure.clj"
    )))


;; incremental testing
;; these test that buffers which are created and then changed are correct.
;; At the moment, this does not check that the changes are actually
;; incremental, cause that's harder.
(defun linked-buffer-test-clone-and-change-with-config
  (filename init &optional f-this f-that retn-that)
  "Clone file and make changes to check incremental updates.
Using INIT clone FILE, then apply F in the buffer, and return the
results."
  ;; most of this is the same as batch-clone..
  (let ((retn nil)
        (f-this
         (or f-this
             (lambda ())))
        (f-that
         (or f-that
             (lambda ()))))
    (with-current-buffer
        (find-file-noselect filename)
      (setq linked-buffer-init init)
      (let ((linked
             (linked-buffer-init-create)))
        (funcall f-this)
        (with-current-buffer
            linked
          (funcall f-that)
          (unless retn-that
            (setq retn
                  (buffer-substring-no-properties
                   (point-min)
                   (point-max))))
          (set-buffer-modified-p nil)
          (kill-buffer)))
      (when retn-that
        (setq retn
              (buffer-substring-no-properties
               (point-min)
               (point-max))))
      (set-buffer-modified-p nil)
      (kill-buffer))
    retn))

(defun linked-buffer-test-clone-and-change-equal
  (init file cloned-file
        &optional f-this f-that retn-that)
  (let ((cloned-file
         (f-read
          (linked-buffer-test-file cloned-file)))
        (cloned-results
         (linked-buffer-test-clone-and-change-with-config
          (linked-buffer-test-file file) init f-this f-that
          retn-that)))
    (if
        (string= cloned-file cloned-results)
        t
      ;; comment this out if you don't want it.
      (linked-buffer-test-equal-loudly cloned-file cloned-results)
      nil)))

(defun linked-buffer-test-clone-and-change-equal-generate
  (init file cloned-file f)
  "Generates the test file for `linked-buffer-test-clone-and-change-with-config'."
  (f-write
   (linked-buffer-test-clone-and-change-with-config
    (linked-buffer-test-file file) init
    f)
   'utf-8
   (concat linked-buffer-test-dir  cloned-file))
  ;; return nil, so that if we use this in a test by mistake, it returns
  ;; false, so there is a good chance it will fail the test.
  nil)

(defvar linked-buffer-test-last-transform "")

(defadvice linked-buffer-insertion-string-transform
  (before store-transform
         (string)
         activate)
  (setq linked-buffer-test-last-transform string))

(ert-deftest linked-buffer-simple-with-change ()
  "Test simple-contents with a change, mostly to check my test machinary."
  (should
   (and
    (equal "simple\nnot simple"
           (linked-buffer-test-clone-and-change-with-config
            (linked-buffer-test-file "simple-contents.txt")
            'linked-buffer-default-init
            (lambda ()
              (goto-char (point-max))
              (insert "not simple"))))
    (equal linked-buffer-test-last-transform "not simple"))))

(ert-deftest linked-buffer-simple-with-change-file()
  "Test simple-contents with a change and compare to file.
This mostly checks my test machinary."
  (should
   (and
    (linked-buffer-test-clone-and-change-equal
     'linked-buffer-default-init
     "simple-contents.txt" "simple-contents-chg.txt"
     (lambda ()
       (goto-char (point-max))
       (insert "simple")))
    (equal linked-buffer-test-last-transform "simple"))))

(ert-deftest linked-buffer-clojure-latex-incremental ()
  (should
   (and
    (linked-buffer-test-clone-and-change-equal
     'linked-buffer-clojure-latex-init
     "block-comment.clj" "block-comment-changed-out.tex"
     (lambda ()
       (forward-line 1)
       (insert ";; inserted\n")))
    (equal linked-buffer-test-last-transform ";; inserted\n")))

  (should
   (and
    (linked-buffer-test-clone-and-change-equal
     'linked-buffer-latex-clojure-init
     "block-comment.tex" "block-comment-changed-1.clj"
     (lambda ()
       (forward-line 1)
       (insert ";; inserted\n")))
    (equal linked-buffer-test-last-transform ";; inserted\n")))

  (should
   (and
    (linked-buffer-test-clone-and-change-equal
     'linked-buffer-latex-clojure-init
     "block-comment.tex" "block-comment-changed-2.clj"
     (lambda ()
       (search-forward "\\begin{code}\n")
       (insert "(form inserted)\n")))
    (equal linked-buffer-test-last-transform "(form inserted)\n"))))

(ert-deftest clojure-latex-first-line ()
  "Tests for a bug after introduction of incremental blocks."
  (should
   (linked-buffer-test-clone-and-change-equal
    'linked-buffer-clojure-latex-init
    "block-comment.clj" "block-comment.tex"
    (lambda ()
      (delete-char 1)
      (delete-char 1)
      (insert ";")
      (insert ";")))))

(ert-deftest orgel-org-incremental ()
  (should
   (linked-buffer-test-clone-and-change-equal
    'linked-buffer-orgel-org-init
    "orgel-org.el" "orgel-org.el"
    nil
    (lambda ()
      (goto-char (point-min))
      (forward-line)
      (insert "a")
      (delete-char -1))
    t)))



    
