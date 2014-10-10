(defun font-candidate (&rest fonts)
  "Return existing font which first match."
  (require 'cl)
  (find-if (lambda (f)
	     (find-font (font-spec :name f))) fonts))

(defun load-directory (dir)
  "Load all files under a given directory."
  (let ((full-path (expand-file-name dir)))
    (if (file-directory-p full-path)
	(dolist (file (directory-files full-path t "\\.el$"))
	  (load-file file)))))
