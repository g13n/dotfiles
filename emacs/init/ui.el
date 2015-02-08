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

;; Emacs scroll settings
(setq scroll-margin 0
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

;; Push the mouse out of the way when the cursor approaches.
(mouse-avoidance-mode 'animate)

;; Do not highlight regions ... ever!
(transient-mark-mode -1)

;; Frame title : set to buffer name
(setq frame-title-format "Emacs - %f ")
(setq icon-title-format  "Emacs - %b")

;; Display line and column numbers in the mode line
(setq line-number-mode 1
      column-number-mode 1)
;; ... and line numbers in the gutter
(linum-mode 1)

;; Highlight words during query replacement
(setq query-replace-highlight t)

;; Highlight matches during search
(setq search-highlight t)

;; Ensure all contents of minibuffer visible
(setq resize-minibuffer-mode t)

;; Never blink cursor, its annoying
(blink-cursor-mode -1)

;; Set the font-size according to the screen resolution
(if window-system
    (progn
      (global-font-lock-mode t)
      (set-default-font (font-candidate "Fira Mono-11"
					"Ubuntu Mono-11"
					"Consolas-11"
					"Source Code Pro-12"
					"PT Mono-11"
					"Anonymous Pro-11"
					"Inconsolata-13"
					"Droid Sans Mono-12"))
      (load-theme 'tango-dark))
  (global-font-lock-mode nil))

;; Get rid of the menu, toolbar since I like a plain looking screen.
;; Lose all UI!
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode)
    (menu-bar-mode -1))

;; This puts the cursor in the last place you edited a particular file.
;; This is very useful for large files.
(require 'saveplace)
(setq-default save-place t)
