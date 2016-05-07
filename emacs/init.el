;; ---------------------------------------------------------------------------
;; Begin Emacs customization
;; ---------------------------------------------------------------------------

;; Package sources
(if (>= (string-to-number emacs-version) 24)
    (progn
      (require 'package)
      (add-to-list 'package-archives
		   '("melpa" . "http://melpa.org/packages/") t)
      (package-initialize)))

;; Core library functions
(load-file (expand-file-name "~/.emacs.d/init/lib.el"))

;; and load all our custom stuff
(load-directory "~/.emacs.d/init")

;; Load custom vendor files
(load-directory "~/.emacs.d/init/vendor")

;; ... finally all programming language modes and tools
(load-file (expand-file-name "~/.emacs.d/init/dev.el"))

(load-file (expand-file-name "~/.emacs.d/init/srv.el"))

;; Load all custom files in sorted order
(load-directory "~/.emacs.d/init/custom")

;; ---------------------------------------------------------------------------
