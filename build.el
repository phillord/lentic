(add-to-list 'load-path default-directory)

(require 'lentic)
(require 'lentic-org)
(require 'lentic-org)
(require 'ox-publish)
(require 'ox-texinfo)
(require 'commander)

(toggle-debug-on-error)

(defun gensource-and-report (file init)
  (message "Cloning %s..."
           file)
  (let ((config
         (lentic-batch-clone-and-save-with-config
          file init)))
    (message "Cloning %s...done" file)))

(defun gensource-gen-if-necessary (file)
  (let* ((target
          (concat
           (file-name-sans-extension file)
           ".org"))
         (locked
          (or (file-locked-p file)
              (file-locked-p target))))
    (if locked
        (message "Skiping %s due to lock %s" file locked)
      (when (file-newer-than-file-p file target)
        (gensource-and-report file 'lentic-orgel-org-init)))))

(defun build/gen-org ()
  (interactive)
  (mapc 'gensource-gen-if-necessary
        (directory-files "." t "^lentic.*.el$")))

(setq org-publish-project-alist
      `(("texinfo"
         :base-directory ,default-directory
         :publishing-directory ,default-directory
         :publishing-function org-texinfo-publish-to-texinfo
         )))

(defun build/gen-texinfo ()
  (message "about to publish")
  (with-current-buffer
      (find-file-noselect "lenticular.org")
    (org-texinfo-export-to-texinfo)))

(defun build/gen-html ()
  (with-current-buffer
      (find-file-noselect "lenticular.org")
    (let ((org-export-htmlize-generate-css 'css))
      (org-html-export-to-html))))



(commander
 (command "gen-org" "Generate org from el" build/gen-org)
 (command "gen-texinfo" "Generate texinfo from org" build/gen-texinfo)
 (command "gen-html" "Generate HTML from org" build/gen-html))
