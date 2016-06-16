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
(eval-after-load 'flycheck
  '((progn )
    (require 'flycheck-google-cpplint)
    ;; Add Google C++ Style checker.
    ;; In default, syntax checked by Clang and Cppcheck.
    (flycheck-add-next-checker 'c/c++-cppcheck
			       '(warnings-only . c/c++-googlelint))))

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
		  js2-basic-offset 4)
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
