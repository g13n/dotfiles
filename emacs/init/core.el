;; Don't show splash screen
(setq inhibit-startup-message t
      inhibit-splash-message t)

;; Visible bell - no beeping!
(setq visible-bell t)

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
(setq backup-by-copying t		      ; don't clobber symlinks
      backup-directory-alist '(("." . "~/.saves")) ; don't litter my fs tree
      delete-old-versions 2
      kept-new-versions 6
      kept-old-versions 2
      version-control t)		; use versioned backups

;; I don't find fundamental mode very useful.  Things generally have a specific
;; mode, or they're text.
(setq default-major-mode 'text-mode)

;; Answer y or n instead of yes or no at minibar prompts.
(defalias 'yes-or-no-p 'y-or-n-p)

;; I use sentences.  Like this.
(setq sentence-end-double-space t)

;; Allow seamless editing of files within archive files.
(auto-compression-mode 1)

;; Always end a file with a newline
(setq require-final-newline t)

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

;; Interactively Do Things
(setq ido-enable-flex-matching t
      ido-everywhere t
      ido-use-filename-at-point 'guess
      ido-create-new-buffer 'always)
(ido-mode 1)

(put 'dired-find-alternate-file 'disabled nil)
