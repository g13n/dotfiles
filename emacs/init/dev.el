;; setup some tools

;; http://www.flycheck.org/manual/latest/index.html
(require 'flycheck)
;; turn on flychecking globally
(add-hook 'after-init-hook #'global-flycheck-mode)
;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

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

;; Required - for Ember
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(setq create-lockfiles nil)

;; JavaScript
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist
	     '("\\.\\(js\\|jsx\\)\\'" . js2-mode))
(add-hook 'js2-mode-hook
          (lambda ()
	    (setq indent-tabs-mode nil
		  js2-bounce-indent-p t
		  js2-basic-offset 2)
	    (local-set-key "\C-x\C-e" 'js-send-last-sexp)
	    (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
	    (local-set-key "\C-cb" 'js-send-buffer)
	    (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
	    (local-set-key "\C-cl" 'js-load-file-and-go)
	    ))

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

;; Project modes
(projectile-global-mode t)

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

;; TypeScript
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1))
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  ;; (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

;; format options
(setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil))

;; Web mode - for all markups
(autoload 'web-mode "web-mode")
(add-to-list 'auto-mode-alist
	     '("\\.\\(html\\|xml\\|xsl\\|xhtml\\)\\'" . web-mode))
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-comment-style 4)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-enable-block-face t)
  (setq web-mode-enable-part-face t)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-current-column-highlight t)
)
(add-hook 'web-mode-hook  'my-web-mode-hook)
