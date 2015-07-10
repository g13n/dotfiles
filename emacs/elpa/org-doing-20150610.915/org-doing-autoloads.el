;;; org-doing-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "org-doing" "org-doing.el" (21902 12535 0 0))
;;; Generated autoloads from org-doing.el

(autoload 'org-doing-log "org-doing" "\
Logs the `description' of what you're doing now in the file
`org-doing-file' at the *top* of the file.

When `later-p' is true, logs the item as something to be done
later.

\(fn DESCRIPTION &optional LATER-P)" t nil)

(autoload 'org-doing "org-doing" "\
Interactive function for running any org-doing command.

The first part of the `command' string is parsed as a command:
- now: calls `org-doing-log'
- later: calls `org-doing-log'
- done: calls `org-doing-done'

\(fn COMMAND)" t nil)

;;;***

;;;### (autoloads nil nil ("org-doing-pkg.el") (21902 12535 461510
;;;;;;  0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; org-doing-autoloads.el ends here
