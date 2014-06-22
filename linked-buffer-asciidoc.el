;; -*- lexical-binding: t -*-

(require 'linked-buffer-block)
(require 'm-buffer)

(defun linked-buffer-asciidoc-commented-new ()
  (linked-buffer-commented-asciidoc-configuration
   "lb-commented-clojure asciidoc"
   :this-buffer (current-buffer)
   :linked-file
   (concat
    (file-name-sans-extension
           (buffer-file-name)) ".adoc")
   :comment ";; "))


(defun linked-buffer-clojure-asciidoc-init ()
  (setq linked-buffer-config
        (linked-buffer-asciidoc-commented-new)))

(defun linked-buffer-asciidoc-uncommented-new ()
  (linked-buffer-uncommented-asciidoc-configuration
   "lb-uncommented-clojure-asciidoc"
   :this-buffer (current-buffer)
   :linked-file
   (concat
    (file-name-sans-extension
     (buffer-file-name)) ".clj")
   :comment ";; "))

(defun linked-buffer-asciidoc-clojure-init ()
  (setq linked-buffer-config
        (linked-buffer-asciidoc-uncommented-new)))

(defclass linked-buffer-commented-asciidoc-configuration
  (linked-buffer-commented-block-configuration)
  ()
  "Linked buffer config for asciidoc and other code.")

(defclass linked-buffer-uncommented-asciidoc-configuration
  (linked-buffer-uncommented-block-configuration)
  ()
  "Linked buffer config for asciidoc and other code")


(defun linked-buffer-splitter (l)
  "Returns a function which for use as a partition predicate.
The returned function returns the first element of L until it is
passed a value higher than the first element, then it returns the
second element and so on."
  #'(lambda (x)
      (when
          (and l
               (< (car l) x))
        (setq l (-drop 1 l)))
      (car l)))

(defun linked-buffer-partition-after-source (l-source l-dots)
  "Given a set of markers l-source, partition the markers in
l-dots."
  (-partition-by
   (linked-buffer-splitter l-source)
   (-drop-while
    (lambda (x)
      (< x (car l-source)))
    l-dots)))


(defun linked-buffer-block-match-asciidoc
  (conf buffer)
  (let* ((source
          (m-buffer-match-begin buffer
                                ";* *\\[source,\\\(clojure\\|lisp\\\)\\]"))
         (dots
          (m-buffer-match buffer
                          "^;* *----"))
         (source-start
          (linked-buffer-partition-after-source
           source
           (m-buffer-match-begin
            dots)))
         (source-end
          (linked-buffer-partition-after-source
           source (m-buffer-match-end dots))))
    (list
     (-map 'cadr source-start)
     (-map 'car source-end))))

(defmethod linked-buffer-block-match
  ((conf linked-buffer-commented-asciidoc-configuration) buffer)
  (linked-buffer-block-match-asciidoc conf buffer))

(defmethod linked-buffer-block-match
  ((conf linked-buffer-uncommented-asciidoc-configuration) buffer)
  (linked-buffer-block-match-asciidoc conf buffer))

(defmethod linked-buffer-invert
  ((conf linked-buffer-commented-asciidoc-configuration))
  (let ((rtn
         (linked-buffer-asciidoc-uncommented-new)))
    (oset rtn :that-buffer (linked-buffer-this conf))
    rtn))

(defmethod linked-buffer-invert
  ((conf linked-buffer-uncommented-asciidoc-configuration))
  (let ((rtn
         (linked-buffer-asciidoc-commented-new)))
    (oset rtn :that-buffer (linked-buffer-this conf))
    rtn))

(provide 'linked-buffer-asciidoc)
