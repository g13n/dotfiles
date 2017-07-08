;;; maker-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "maker-mode" "maker-mode.el" (22523 25829 0
;;;;;;  0))
;;; Generated autoloads from maker-mode.el

(autoload 'maker-start "maker-mode" "\
Start maker

\(fn)" t nil)

(autoload 'maker-command "maker-mode" "\
Send a command to the maker process of the current buffer's maker project.
Prompts for the command to send when in interactive mode.

This command does the following:
  - displays the buffer without moving focus to it
  - erases the buffer
  - forgets about compilation errors

\(fn COMMAND)" t nil)

(autoload 'maker-run-previous-command "maker-mode" "\
Repeat the command that was previously executed (or run the
maker:default-command, if no other command has yet been run).

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; maker-mode-autoloads.el ends here
