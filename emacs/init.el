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
 '(custom-safe-themes
   (quote
    ("9be1d34d961a40d94ef94d0d08a364c3d27201f3c98c9d38e36f10588469ea57" "98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6" "78c1c89192e172436dbf892bd90562bc89e2cc3811b5f9506226e735a953a9c6" default)))
 '(package-selected-packages
   (quote
    (base16-theme ensime maker-mode scala-mode yaml-mode websocket web-mode vagrant tide solarized-theme soft-charcoal-theme scss-mode rcirc-notify rcirc-alert projectile php-mode org-doing org-dashboard monky markdown-mode magit-tramp magit-gitflow magit-filenotify js2-mode js-doc highlight-thing hide-comnt handlebars-mode haml-mode grunt google-c-style git-blame fringe-helper flymake-yaml flymake-shell flymake-json flymake-google-cpplint flymake-css exec-path-from-shell ember-mode emacsql-mysql dockerfile-mode docker dired-single company comment-dwim-2 autopair auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
