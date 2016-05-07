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

(defun mode-line-visual-toggle ()
  (interactive)
  (let ((faces-to-toggle '(mode-line mode-line-inactive))
        (invisible-color "#e8e8e8")
        (visible-color "#a1b56c"))
    (cond ((string= visible-color (face-attribute 'mode-line :background))
           (mapcar (lambda (face)
                     (set-face-background face invisible-color)
                     (set-face-attribute face nil :height 20))
                   faces-to-toggle))
          (t
           (mapcar (lambda (face)
                     (set-face-background face visible-color)
                     (set-face-attribute face nil :height (face-attribute 'default :height)))
                   faces-to-toggle)))))
