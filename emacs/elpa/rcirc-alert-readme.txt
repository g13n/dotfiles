This code implements a notification (alert) library on top of emacs rcirc IRC client.

This code integrates part of the original sources. It departs from the original in two ways

- alerts are considered as a trigger action; any trigger action executes one script file (or anything else)

- five different types of alerts are considered here, three of them may be disabled

  "message" - someone talks to the user by classical "user_nick: ... " (always enabled)
  "keyword" - keyword detected
  "private" - private message (always enabled)
  "nick"    - given nicks changes status (join, ...)
  "always"  - alert of any message

As long as this code only triggers an action, an example a bash script ('rcirc-alert.sh') is necessary
the alerts. This script will create a notification or whatever the user needs. It accepts four input
arguments, being the first argument the alert type. Refer to the example provided script file for more details.
This scripts provides an example configuration file showing a setup and how to use this library.

Useful variables to customize alerts are described in the *Variables section of this file.

** Messages
** Enables
** Action script
** Lists of alert triggers

These may be used in a per-buffer basis to specify individual needs on given chats (notify anything on twitter channel, alert when
given nick appears online at #emacs, keywords on #archlinux, etc.)

TODO

Code


(require 'rcirc)

* GROUP

(defgroup rcirc-alert nil
  "Alerts thought a script for rcirc irc mode."
  :group 'rcirc)

* Variables

** Alert Messages
Text to be displayed in the alert for each type of notification

(defcustom rcirc-alert-message-message "%s mentioned you: %s"
  "Format of the message to display in the popup.
The first %s will expand to the nick that notified you,
the second %s (if any) will expand to the message text itself."
  :type 'string
  :group 'rcirc-alert)

(defcustom rcirc-alert-message-keyword "%s mentioned the keyword(s) %s: %s"
  "Format of the message to display in the popup.
The first %s will expand to the nick that mentioned the keyword,
the second %s (if any) will expand to the keyword used,
the third %s (if any) will expand to the message text itself.
This only happens if `rcirc-enable-alert-keywords' is non-nil."
  :type 'string
  :group 'rcirc-alert)

(defcustom rcirc-alert-message-nick "Nick  %s  has %s"
  "Format of the message to display in the popup.
The first %s will expand to the nick that changed status,
the second %s (if any) will expand to the new status,
This only happens if `rcirc-enable-alert-nick' is non-nil."
  :type 'string
  :group 'rcirc-alert)

(defcustom rcirc-alert-message-private "%s sent a private message: %s"
  "Format of the message to display in the popup.
The first %s will expand to the nick that notified you,
the second %s (if any) will expand to the message text itself."
  :type 'string
  :group 'rcirc-alert)

(defcustom rcirc-alert-message-always "%s sent a message: %s"
  "Format of the message to display in the popup.
The first %s will expand to the nick that notified,
the second %s (if any) will expand to the message text itself,
This only happens if `rcirc-enable-alert-always' is non-nil."
  :type 'string
  :group 'rcirc-alert)

** Enable / Disable Alerts
"message" and "private" are always enabled, the remaining may be disabled

(defcustom rcirc-enable-alert-keywords t
  "Non-nil means matches of `rcirc-alert-keywords' will result in alert.
See `rcirc-alert-message-keyword' for the message format to use."
  :type 'boolean
  :group 'rcirc-alert)

(defcustom rcirc-enable-alert-nick t
  "Non-nil means matches of `rcirc-alert-nicks' will result in alert.
See `rcirc-alert-message-nick' for the message format to use."
  :type 'boolean
  :group 'rcirc-alert)

(defcustom rcirc-enable-alert-always t
  "Non-nil means any new message will result in notification."
  :type 'boolean
  :group 'rcirc-alert)

** Action script

(defcustom my-rcirc-notification-script (concat user-emacs-directory "rcirc-alert.sh")
  "Script to trigger when a notification condition is met.
This script manages all types of notifications internally following its first argument."
  :type 'string
  :group 'rcirc-alert)

** Lists of alert triggers

(defcustom rcirc-alert-nicks nil
  "List of nicks whose new status triggers a notification of type 'nick'"
  :type '(repeat string)
  :group 'rcirc-alert)

(defcustom rcirc-alert-keywords nil
  "List of keywords that trigger a notification of type 'keyword'"
  :type '(repeat string)
  :group 'rcirc-alert)

**

(defvar rcirc-alert-nick-alist nil
  "An alist of nicks and the last time they tried to trigger a
notification.")

(defcustom rcirc-alert-timeout 60
  "Number of seconds that will elapse between notifications from the
same person."
  :type 'integer
  :group 'rcirc-alert)


* Notification Action
Comment

(defun my-page-me-notify (type title msg)
  "If notification script is in path, run it for this type of notification
."
  (cond
   ((executable-find my-rcirc-notification-script)
      ;; (start-process "page-me" nil my-rcirc-notification-script type title msg (substring (buffer-name) 0 (string-match-p (regexp-quote "@") (buffer-name))))
    (if rcirc-target
      (start-process "page-me" nil my-rcirc-notification-script type title msg rcirc-target)
        )
    )
    (t (error "No method available to page you."))))


* Notificators
wrappers around the notification action

(defun my-rcirc-alert-message (sender &optional text)
  (when window-system
    ;; Set default dir to appease the notification gods
    (let ((default-directory "~/"))
      (my-page-me-notify "message" "rcIRC Message" (format rcirc-alert-message-message (upcase sender) text)))))

(defun my-rcirc-alert-keyword (sender &optional keyword text)
  (when window-system
    ;; Set default dir to appease the notification gods
    (let ((default-directory "~/"))
      (if (listp keyword)
          (setq keyword (mapconcat 'identity keyword ", ")))
      (my-page-me-notify "keyword" "rcIRC KeyWord" (format rcirc-alert-message-keyword (upcase sender) (upcase keyword) text)))))

(defun my-rcirc-alert-private (sender &optional text)
  (when window-system
    ;; Set default dir to appease the notification gods
    (let ((default-directory "~/"))
      (my-page-me-notify "private" "rcIRC Private Message" (format rcirc-alert-message-private (upcase sender) text)))))

(defun my-rcirc-alert-nick (sender &optional keyword)
  (when window-system
    ;; Set default dir to appease the notification gods
    (let ((default-directory "~/"))
      (if (listp keyword)
          (setq keyword (mapconcat 'identity keyword ", "))
        )
      (my-page-me-notify "nick" "rcIRC New Nick Status" (format rcirc-alert-message-nick (upcase sender) keyword)))))

(defun my-rcirc-alert-always (sender &optional text)
  (when window-system
    ;; Set default dir to appease the notification gods
    (let ((default-directory "~/"))
      (my-page-me-notify "always" "rcIRC Message" (format rcirc-alert-message-always (upcase sender) text)))))


* Allowed Senders
Comment

(defun my-rcirc-alert-allowed (nick &optional delay)
  "Return non-nil if a notification should be made for NICK.
If DELAY is specified, it will be the minimum time in seconds
that can occur between two notifications.  The default is
`rcirc-alert-timeout'."
  (unless delay (setq delay rcirc-alert-timeout))
  (let ((cur-time (float-time (current-time)))
        (cur-assoc (assoc nick rcirc-alert-nick-alist))
        (last-time))
    (if cur-assoc
        (progn
          (setq last-time (cdr cur-assoc))
          (setcdr cur-assoc cur-time)
          (> (abs (- cur-time last-time)) delay))
      (push (cons nick cur-time) rcirc-alert-nick-alist)
      t)))


* Criteria to trigger notifications

** notifications 1 and 2 : message and keyword
(defun rcirc-alert-message (proc sender response target text)
  "Notify the current user when someone sends a message that
matches the current nick or keywords."
  (interactive)
  (when (and (not (string= (rcirc-nick proc) sender))
             (not (string= (rcirc-server-name proc) sender)))
    (cond
     ;; Cond 1 :
     (
      (and (string-match (concat "\\b" (rcirc-nick proc) "\\b") text)
           (my-rcirc-alert-allowed sender))
      (my-rcirc-alert-message sender text)
      )
     ;; Cond 1
     ;; Cond 2 : keyword
     (rcirc-enable-alert-keywords
      (let (keywords)
        (dolist (key rcirc-alert-keywords keywords)
          (when (string-match (concat "\\<" key "\\>")
                              text)
            (push key keywords)))
        (when keywords
          (if (my-rcirc-alert-allowed sender)
              (my-rcirc-alert-keyword sender keywords text))
          )
        )
      )
     ;; Cond 2
     ) ;; cond
    ) ;; when
  )

** notification 3 (private message)
(defun rcirc-alert-private (proc sender response target text)
  "Notify the current user when someone sends a private message
to him."
  (interactive)
  (when (and (string= response "PRIVMSG")
             (not (string= sender (rcirc-nick proc)))
             (not (rcirc-channel-p target))
             (my-rcirc-alert-allowed sender))
    (my-rcirc-alert-private sender text)
    ))

** notification 4 (new nick status)
(defun rcirc-alert-nick (proc sender response target text)
  "Notify the current a nick in the list changes status."
  (interactive)
  (when (and (not (string= (rcirc-nick proc) sender))
             (not (string= (rcirc-server-name proc) sender))
             ;;not nil sender
             (not (string= nil sender))
             )
    (cond
     ;; Cond
     (rcirc-enable-alert-nick
      ;; use the member function
      (let (keywords)
        (dolist (key rcirc-alert-nicks keywords)
          (when
              (and
               ;; when the sender is in the list rcirc-alert-nicks
               ;; (string-match (concat "\\<" key "\\>") sender)
               (string-match key sender)
               ;; when it changes to these states
               (member response '("QUIT" "PART" "JOIN" "AWAY"))
               )
            (progn
              (push key keywords)
              ;; (message response)
              )
            ;; (push key keywords)
            )
          ) ;; dolist
        (when keywords
          (if (my-rcirc-alert-allowed sender)
              (my-rcirc-alert-nick sender response))
          )
        )
      )
     ;; Cond
     ) ;; cond
    ) ;; when
  )

** notification 5 : notificate always
(defun rcirc-alert-always (proc sender response target text)
  "
    ."
  (interactive)
  (when (and (not (string= (rcirc-nick proc) sender))
             (not (string= (rcirc-server-name proc) sender)))
    (cond
     (rcirc-enable-alert-always
      (my-rcirc-alert-always sender text)
      ) ;; cond
     ) ;; cond
    ) ;; when
  )


* Hooks
When something gets printed, trigger an action

Message and keyword notification (1 & 2)
(add-hook 'rcirc-print-functions 'rcirc-alert-message)

Private message notification (3)
(add-hook 'rcirc-print-functions 'rcirc-alert-private)

Nick status change notification (4)
(add-hook 'rcirc-print-functions 'rcirc-alert-nick)

Always notification (5)
(add-hook 'rcirc-print-functions 'rcirc-alert-always)

(provide 'rcirc-alert)

rcirc-alert.el ends here
