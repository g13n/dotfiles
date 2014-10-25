;;; rcirc-notify-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (rcirc-notify-add-hooks rcirc-notify-privmsg rcirc-notify-me)
;;;;;;  "rcirc-notify" "rcirc-notify.el" (21579 13447 402489 19000))
;;; Generated autoloads from rcirc-notify.el

(autoload 'rcirc-notify-me "rcirc-notify" "\
Notify the current user when someone sends a message that
matches the current nick.

\(fn PROC SENDER RESPONSE TARGET TEXT)" t nil)

(autoload 'rcirc-notify-privmsg "rcirc-notify" "\
Notify the current user when someone sends a private message
to them.

\(fn PROC SENDER RESPONSE TARGET TEXT)" t nil)

(autoload 'rcirc-notify-add-hooks "rcirc-notify" "\
Initialize rcirc-notify into rcirc with hooks.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("rcirc-notify-pkg.el") (21579 13447 430904
;;;;;;  133000))

;;;***

(provide 'rcirc-notify-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; rcirc-notify-autoloads.el ends here
