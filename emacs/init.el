;; ----------------------------------------------------------------------------
;; This is divided into three sections.
;;
;; The first section sets and modifies Emacs directly, invoking and tweaking
;; packages and settings that come with your vanilla Emacs CVS, such as
;; scrolling or changing key bindings.
;;
;; The second section loads all my downloaded elisp files, and sets them up.
;;
;; The third section contains the customised settings.
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;; Utility methods
;; ----------------------------------------------------------------------------

(require 'cl)
(defun font-candidate (&rest fonts)
  "Return existing font which first match."
  (find-if (lambda (f) (find-font (font-spec :name f))) fonts))

;; ----------------------------------------------------------------------------
;; Customise the general settings
;; ----------------------------------------------------------------------------

;; Don't show splash screen
(setq inhibit-startup-message t
      inhibit-splash-message t)

;; Visible bell - no beeping!
(setq visible-bell t)

;; Modify the mode-line as well.  This is a cleaner setup than the default
;; settings for the mode-line.
(setq default-mode-line-format
      '("-"
        mode-line-mule-info
        mode-line-modified
        mode-line-frame-identification
        mode-line-buffer-identification
        "  "
        global-mode-string
        "   %[(" mode-name mode-line-process minor-mode-alist "%n"
        ")%]--"
        (line-number-mode "L%l--")
        (column-number-mode "C%c--")
        (-3 . "%p")
        "-%-"))

(setq savehist-additional-variables        ; also save...
      '(search-ring regexp-search-ring)    ; ... my search entries
      savehist-file "~/.emacs.d/savehist") ; keep my home clean
(savehist-mode t)                          ; do customisation before activate

;; This sets garbage collection to hundred times of the default.  Supposedly
;; significantly speeds up startup time.  Seems to work for me,  but my
;; computer is an AMD quad core monster!
(setq gc-cons-threshold 50000000)

;; Don't echo passwords when communicating with interactive programs - basic
;; security.
(add-hook 'comint-output-filter-functions
          'comint-watch-for-password-prompt)

;; Use ANSI colors within shell-mode
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Change pasting behavior.  Normally, it pastes where the mouse is at, which
;; is not necessarily where the cursor is.  This changes things so all pastes,
;; whether they be middle-click or C-y or menu, all paste at the cursor.
(setq mouse-yank-at-point t)

;; While we are at it, always flash for parens.
(show-paren-mode 1)
(global-set-key "%" 'match-paren)
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
	((looking-at "\\s\)") (forward-char 1) (backward-list 1))
	(t (self-insert-command (or arg 1)))))

;; The autosave is typically done by keystrokes, but I'd like to save after a
;; certain amount of time as well.
(setq auto-save-timeout 1800)

;; Change backup behavior to save in a directory, not in a miscellany of files
;; all over the place.
(setq
 backup-by-copying t                          ; don't clobber symlinks
 backup-directory-alist '(("." . "~/.saves")) ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)                     ; use versioned backups

;; This starts up a server automatically, so it will be running to accept
;; things like wikipedia articles and other text areas.
(server-start)

;; I don't find fundamental mode very useful.  Things generally have a specific
;; mode, or they're text.
(setq default-major-mode 'text-mode)

;; Answer y or n instead of yes or no at minibar prompts.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Emacs scroll settings
(setq scroll-margin 0
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

;; Push the mouse out of the way when the cursor approaches.
(mouse-avoidance-mode 'jump)

;; I use sentences.  Like this.
(setq sentence-end-double-space t)

;; Allow seamless editing of files within archive files.
(auto-compression-mode 1)

;; Do not highlight regions ... ever!
(transient-mark-mode -1)

;; Always end a file with a newline
(setq require-final-newline t)

;; Frame title : set to buffer name
(setq frame-title-format "Emacs - %f ")
(setq icon-title-format  "Emacs - %b")

;; Display line and column numbers in the mode line
(setq line-number-mode 1
      column-number-mode 1)

;; Highlight words during query replacement
(setq query-replace-highlight t)

;; Highlight matches during search
(setq search-highlight t)

;; Ensure all contents of minibuffer visible
(setq resize-minibuffer-mode t)

;; Kill whole line (including the newline)
(setq kill-whole-line t)

;; Always kill/yank to/from clipboard
(menu-bar-enable-clipboard)
(setq x-selection-timeout 0)
(setq x-select-enable-clipboard t)

;; Enable some useful commands by default
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(setq special-display-buffer-names '("*Shell Command Output*"))

;; Reload buffers automatically when they change on disk
(global-auto-revert-mode t)

;; Remove trailing whitespaces on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Save and restore session on startup/shutdown
(setq desktop-path '("~/.emacs.d"))
(desktop-save-mode 1)

;; Add /usr/local/bin to executable path
(add-to-list 'exec-path '"/usr/local/bin" t)

;; ----------------------------------------------------------------------------
;; Customize additional module libraries
;; ----------------------------------------------------------------------------

;(require 'helm-config)
;(helm-mode 1)

;; ----------------------------------------------------------------------------
;; Load all sub modules
;; ----------------------------------------------------------------------------

;; C/C++
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;; JavaScript
(autoload 'js2-mode "js2-mode" nil t)
(setq auto-mode-alist
      (cons '("\\.\\(js\\|jsx\\)\\'" . js2-mode)
	    auto-mode-alist))
(add-hook 'js2-mode-hook
          (lambda () (setq indent-tabs-mode nil)))

;; nXML - XML editing
(load-library "nxml-mode")
(setq auto-mode-alist
      (cons '("\\.\\(html\\|xml\\|xsl\\|rng\\|xhtml\\)\\'" . nxml-mode)
            auto-mode-alist))
(defun custom-nxml ()
  (setq nxml-child-indent 4)
  (setq nxml-slash-auto-complete-flag t)
  (setq indent-tabs-mode nil)
  (nxml-bind-meta-tab-to-complete-flag t)
  (nxml-slash-auto-complete-flag t)
  (unify-8859-on-decoding-mode)
  (auto-fill-mode -1))
(add-hook 'nxml-mode-hook 'custom-nxml)

;; Org Mode
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(setq org-log-done 'note)
(setq org-todo-keywords
      '((sequence "TODO(t)" "INPROGRESS(p)" "HOLD(h@/!)"
                  "|" "DONE(d!)" "CANCELLED(c@/!)")))

;; PHP Mode
(setq auto-mode-alist
      (cons '("\\.\\(php\\|inc\\)\\'" . php-mode) auto-mode-alist))

;; Tramp - Transparent access to remote systems
(require 'tramp)

;; Load some utilities
(add-to-list 'load-path (expand-file-name "~/.emacs.d/utils"))
(require 'phonetic)

;; ----------------------------------------------------------------------------
;; More customisation
;; ----------------------------------------------------------------------------

;; This puts the cursor in the last place you edited a particular file.  This
;; is very useful for large files.
(require 'saveplace)
(setq-default save-place t)

;; ----------------------------------------------------------------------------
;; UI customisation
;; ----------------------------------------------------------------------------

;; Never blink cursor, its annoying
(blink-cursor-mode -1)

;; Set the font-size according to the screen resolution
(if window-system
    (progn
      (global-font-lock-mode t)
      (set-default-font (font-candidate "Source Code Pro-11"
					"Ubuntu Mono-13"
					"Inconsolata-13"
					"Droid Sans Mono-12"
					"Consolas-13")))
  (global-font-lock-mode nil))

;; Get rid of the menu, toolbar since I like a plain looking screen.
;; Lose all UI!
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(defun switch-full-screen ()
  (interactive)
  (shell-command "wmctrl -r :ACTIVE: -btoggle,fullscreen"))

;; Keyboard preferences

(global-set-key "\C-w" 'backward-kill-word) ; this is faster than backspace
(global-set-key "\C-x\C-k" 'kill-region)    ; formerly bound to \C-w
(global-set-key "\C-c\C-k" 'kill-region)    ; formerly bound to \C-w
(global-set-key [f11] 'switch-full-screen)

(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

(global-set-key (kbd "<f8> w") 'whitespace-mode)
(global-set-key (kbd "<f8> c") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "<f8> r") 'replace-string)
(global-set-key (kbd "<f8> R") 'replace-regexp)
(global-set-key (kbd "<f8> a") 'ack)

;; Set the theme
(if (< (string-to-number emacs-version) 24)
    (add-to-list 'load-path (expand-file-name "~/.emacs.d/themes")))
(if (>= (string-to-number emacs-version) 24)
    (progn
      (add-to-list 'custom-theme-load-path (expand-file-name "~/src/build/tomorrow-theme/GNU Emacs"))
      (add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/themes/color-theme-radiance"))
      ))

;; RCIRC
(require 'tls)

(require 'rcirc)

(setq rcirc-default-nick "g13n"
      rcirc-default-full-name "Gopal Venkatesan")

(setq rcirc-server-alist
      '(("irc.freenode.net"
	 :port 6697
	 :encryption tls
	 :channels ("#emacs" "#reactjs")
	 ))
	)

;; Package sources
(if (>= (string-to-number emacs-version) 24)
    (progn
      (require 'package)
      (add-to-list 'package-archives
		   '("melpa" . "http://melpa.milkbox.net/packages/") t)
      (package-initialize)
      ))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("e3a3b7d7fe89b5d57d40bc825ca2324875a6f37bd63da66f2a6fc68cc8b2ee95" "53e29ea3d0251198924328fd943d6ead860e9f47af8d22f0b764d11168455a8e" "0795e2c85394140788d72d34969be4acb305e4a54149e7237787d9df27832fbb" "53c542b560d232436e14619d058f81434d6bbcdc42e00a4db53d2667d841702e" "bd115791a5ac6058164193164fd1245ac9dc97207783eae036f0bfc9ad9670e0" "a99e7c91236b2aba4cd374080c73f390c55173c5a1b4ac662eeb3172b60a9814" "fc2782b33667eb932e4ffe9dac475f898bf7c656f8ba60e2276704fabb7fa63b" "3b819bba57a676edf6e4881bd38c777f96d1aa3b3b5bc21d8266fa5b0d0f1ebf" "60f04e478dedc16397353fb9f33f0d895ea3dab4f581307fbf0aa2f07e658a40" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "3c708b84612872e720796ea1b069cf3c8b3e909a2e1da04131f40e307605b7f9" "572caef0c27b100a404db8d540fd5b31397f90ab660ef5539ff0863ff9bee26a" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(put 'dired-find-alternate-file 'disabled nil)

(eval-after-load 'rcirc '(require 'rcirc-notify))

(load-theme 'radiance)
