;; setup some tools
(require 'autopair)

(add-to-list 'load-path
	     (expand-file-name "~/.emacs.d/utils"))

;; C/C++
(add-hook 'c-mode-common-hook
	  'google-set-c-style)
(add-hook 'c-mode-common-hook
	  'google-make-newline-indent)
(add-hook 'c-mode-common-hook
	  #'(lambda ()
	      (autopair-mode)))

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
(autoload 'js2-mode "js2-mode" nil t)
(setq auto-mode-alist
      (cons '("\\.\\(js\\|jsx\\)\\'" . js2-mode)
	    auto-mode-alist))
(add-hook 'js2-mode-hook
          (lambda ()
	    (autopair-mode)
	    (setq indent-tabs-mode nil
		  js2-basic-offset 2)))

;; nXML - XML editing
(load-library "nxml-mode")
(setq auto-mode-alist
      (cons '("\\.\\(html\\|xml\\|xsl\\|rng\\|xhtml\\)\\'" . nxml-mode)
            auto-mode-alist))
(defun custom-nxml ()
  (setq nxml-child-indent 2)
  (setq nxml-slash-auto-complete-flag t)
  (setq indent-tabs-mode nil)
  (setq nxml-bind-meta-tab-to-complete-flag t)
  (setq nxml-slash-auto-complete-flag t)
  (unify-8859-on-decoding-mode)
  (auto-fill-mode -1)
  (autopair-mode))
(add-hook 'nxml-mode-hook 'custom-nxml)

;; Org Mode
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(setq org-log-done 'note)
(setq org-todo-keywords
      '((sequence "TODO(t)" "INPROGRESS(p)" "HOLD(h@/!)"
                  "|" "DONE(d!)" "CANCELLED(c@/!)")))

;; PHP
(setq auto-mode-alist
      (cons '("\\.\\(php\\|inc\\)\\'" . php-mode) auto-mode-alist))

;; Tramp - Transparent access to remote systems
(require 'tramp)

;; Code Snippets!
(require 'yasnippet)
(yas-global-mode 1)
