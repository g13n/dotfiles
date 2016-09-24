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

;; https://github.com/purcell/exec-path-from-shell
;; only need exec-path-from-shell on OSX
;; this hopefully sets up path and other vars better
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; ---------------------------------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
