(require 'linked-buffer)


(defvar linked-buffer-test-dir
  (concat
   (file-name-directory
    (locate-library "linked-buffer.el"))
   "test/"))

(defun linked-buffer-test-file (filename)
  (concat linked-buffer-test-dir filename))

(defvar conf-default
  (linked-buffer-default-configuration "bob"))

(ert-deftest linked-buffer-conf ()
  (should
   (equal 'fundamental-mode
          (oref conf-default :linked-mode))))

(ert-deftest linked-buffer-simple ()
  (should
   (equal "simple\n"
          (linked-buffer-batch-clone-with-config
           (linked-buffer-test-file "simple-contents.txt")
           'linked-buffer-default-init))))
