(require 'eww)
(require 'browse-url)
(require 'lentic)
(require 'f)

;; ** Orgify Package
(defun lentic-doc-all-files-of-package (package)
  "Fetch all the files that are part of package.
This function assumes that all the files are in one place and
follow the standard naming convention of using the package name
as a prefix. "
  (let* ((main-file
          (locate-library package))
         (dir
          (f-parent main-file))
         (others
          (f-glob
           (concat dir "/" package "*.el"))))
    others))

(defun lentic-doc-orgify-if-necessary (file)
  (let* ((target
          (concat
           (file-name-sans-extension file)
           ".org"))
         (locked
          (or (file-locked-p file)
              (file-locked-p target)))
         (open
          (or
           (get-file-buffer file)
           (get-file-buffer target))))
    (unless (or locked open)
      (when (file-newer-than-file-p file target)
        (let ((lentic-kill-retain t))
          (lentic-batch-clone-and-save-with-config
           file 'lentic-orgel-org-init))))))

(defun lentic-doc-orgify-all-if-necessary (files)
  (-map 'lentic-doc-orgify-if-necessary files))

(defun lentic-doc-orgify-package (package)
  (lentic-doc-orgify-all-if-necessary
   (lentic-doc-all-files-of-package
    (symbol-name package))))

;; ** htmlify package
(defun lentic-doc-htmlify-package (package start)
  (lentic-doc-orgify-package package)
  (with-current-buffer
      (find-file-noselect
       (concat
        (f-parent (locate-library (symbol-name package)))
        "/"
        start))
    (let ((org-export-htmlize-generate-css 'css))
      (org-html-export-to-html))))


;; ** lentic self-doc
;;;###autoload
(defun lentic-doc-generate-self ()
  (interactive)
  (lentic-doc-htmlify-package 'lentic "lenticular.org"))

(defvar lentic-doc-file
  (concat
   (f-parent
    (locate-library "lentic.el"))
   "/" "lenticular.html"))

(defun lentic-doc-attempt-doc-view ()
  (eww-open-file lentic-doc-file))

(defun lentic-doc-ensure-doc ()
  (unless (f-exists? lentic-doc-file)
    (lentic-doc-generate-self)))

;;;###autoload
(defun lentic-doc-eww-view ()
  (interactive)
  (lentic-doc-ensure-doc)
  (lentic-doc-attempt-doc-view))

;;;###autoload
(defun lentic-doc-external-view ()
  (interactive)
  (lentic-doc-ensure-doc)
  (browse-url-of-file lentic-doc-file))

(provide 'lentic-doc)
