(require 'linked-buffer)


(defvar conf-default
  (linked-buffer-default-configuration "bob"))

(ert-deftest linked-buffer-conf ()
  (should
   (equal 'fundamental-mode
          (oref conf-default :linked-mode))))
