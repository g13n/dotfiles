;; setup some tools

(add-to-list 'load-path
	     (expand-file-name "~/.emacs.d/utils"))

;; C/C++
(add-hook 'c-mode-common-hook
	  'google-set-c-style)
(add-hook 'c-mode-common-hook
	  'google-make-newline-indent)
(add-hook 'c-mode-common-hook
	  #'(lambda ()
	      (electric-pair-mode t)))

;; Comments
(global-set-key (kbd "M-;") 'comment-dwim-2)
(setq comment-dwim-2--inline-comment-behavior 'reindent-comment)

;; Show FIXME/TODO/BUG/KLUDGE comments
(require 'fic-mode)
(add-hook 'c++-mode-hook 'turn-on-fic-mode)
(add-hook 'js3-mode-hook 'turn-on-fic-mode)

;; Java
(defun malabar-mode-bootstrap ()
  (require 'cedet)
  (require 'semantic)
  (load "semantic/loaddefs.el")
  (semantic-mode 1);;
  (require 'malabar-mode)
  (load "malabar-flycheck")
  (malabar-mode)
  (flycheck-mode))

(add-to-list 'auto-mode-alist
	     '("\\.java\\'" . malabar-mode-bootstrap))

;; JavaScript
(autoload 'js3-mode "js3-mode" nil t)
(add-to-list 'auto-mode-alist
	     '("\\.\\(js\\|jsx\\)\\'" . js3-mode))
(add-hook 'js3-mode-hook
          (lambda ()
	    (setq indent-tabs-mode nil
		  js3-auto-indent-p t
		  js3-enter-indents-newline t
		  js3-indent-on-enter-key t)))

;; nXML - XML editing
(autoload 'nxml-mode "nxml-mode")
(add-to-list 'auto-mode-alist
	     '("\\.\\(html\\|xml\\|xsl\\|rng\\|xhtml\\)\\'" . nxml-mode))
(defun custom-nxml ()
  (setq nxml-child-indent 2)
  (setq nxml-slash-auto-complete-flag t)
  (setq indent-tabs-mode nil)
  (setq nxml-bind-meta-tab-to-complete-flag t)
  (setq nxml-slash-auto-complete-flag t)
  (unify-8859-on-decoding-mode)
  (auto-fill-mode -1)
  (electric-pair-mode t))
(add-hook 'nxml-mode-hook 'custom-nxml)

;; Org Mode
(add-to-list 'auto-mode-alist
	     '("\\.org$" . org-mode))
(setq org-log-done 'note)
(setq org-todo-keywords
      '((sequence "TODO(t)" "INPROGRESS(p)" "HOLD(h@/!)"
                  "|" "DONE(d!)" "CANCELLED(c@/!)")))

;; PHP
(autoload 'php-mode "php-mode")
(add-hook 'php-mode-hook
	  (lambda()
	    (electric-pair-mode t)
	    (setq indent-tabs-mode nil)))
(add-to-list 'auto-mode-alist
	     '("\\.\\(php\\|inc\\)\\'" . php-mode))

;; SCSS
(autoload 'scss-mode "scss-mode")
(add-hook 'scss-mode-hook
	  (lambda()
	    (electric-pair-mode t)
	    (setq
	     scss-compile-at-save nil
	     indent-tabs-mode nil)))
(add-to-list 'auto-mode-alist
	     '("\\.scss\\'" . scss-mode))

;; Tramp - Transparent access to remote systems
(require 'tramp)

;; Code Snippets!
(require 'yasnippet)
(yas-global-mode 1)

;; Project modes
(projectile-global-mode t)
