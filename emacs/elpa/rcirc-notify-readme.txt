This code is inspired in part by erc-page-me.el and offers
the same functionality as it, but for rcirc.

* `rcirc-notify-message` contains the message contents for
   the notification

* `rcirc-notify-message-private` contains the message
   contents for a private message notification

* `rcirc-notify-nick-alist` is a list containing the last
   folks who notified you, and what times they did it at

* `rcirc-notify-timeout` controls the number of seconds
   in between notifications from the same nick.

Grow For Windows
Run something like this from eshell before you use rcirc-notify:
/Programme/Growl\ for\ Windows/growlnotify.com /t:IRC \
/ai:http://www.emacswiki.org/pics/static/CarbonEmacsPackageIcon.png \
/a:Emacs /r:IRC /n:IRC foo

(require 'rcirc)
(require 'cl) ;; needed for 'some'

(defgroup rcirc-notify nil
  "Notifications for the rcirc IRC client."
  :group 'rcirc
  )

(defcustom rcirc-notify-message "%s mentioned you: %s"
  "Format of the message to display in the popup.
The first %s will expand to the nick that notified you,
the second %s (if any) will expand to the message text itself."
  :type '(string)
  :group 'rcirc-notify
  )

(defcustom rcirc-notify-keywords t
  "Non-nil means matches of `rcirc-keywords' will result in notification.
See `rcirc-notify-keyword' for the message format to use."
  :type '(boolean)
  :group 'rcirc-notify
  )

(defcustom rcirc-notify-check-frame nil
  "When a notify happens check if RCIRC buffer is open in a frame.
If you don't want notifications if you have rcirc open in a frame
then turn this on and they won't be delivered."
  :type '(boolean)
  :group 'rcirc-notify)

(defcustom rcirc-notify-keyword "%s mentioned the keyword '%s': %s"
  "Format of the message to display in the popup.
The first %s will expand to the nick that mentioned the keyword,
the second %s (if any) will expand to the keyword used,
the third %s (if any) will expand to the message text itself.
This only happens if `rcirc-notify-keywords' is non-nil."
  :type '(string)
  :group 'rcirc-notify
  )

(defcustom rcirc-notify-message-private "%s sent a private message: %s"
  "Format of the message to display in the popup.
The first %s will expand to the nick that notified you,
the second %s (if any) will expand to the message text itself."
  :type '(string)
  :group 'rcirc-notify
  )

(defcustom rcirc-notify-popup-timeout 8640000
  "Number of seconds to show the notifcation popup, if relevant.
If the notification is done via an operating system popup message
then this controls the timeout of that popup."
  :type '(integer)
  :group 'rcirc-notify
  )

(defcustom rcirc-notify-timeout 60
  "Seconds between notifications from the same person."
  :type '(integer)
  :group 'rcirc-notify
  )

(defvar rcirc-notify--nick-alist nil
  "An alist of nicks and the last time they tried to trigger a notification."
  )



(defun rcirc-notify-page-me (msg)
  (cond
    ((executable-find "notify-send")
     (start-process "page-me" nil
                    ;; 8640000 ms = 1 day
                    "notify-send" "-u" "normal" "-i" "gtk-dialog-info"
                    "-t" (format "%s" rcirc-notify-popup-timeout) "rcirc"
                    msg))
    ((executable-find "terminal-notify")
     (start-process "page-me" "*debug*" "terminal-notify" "-activate" "org.gnu.Emacs" "-message" msg))
    ((executable-find "growlnotify.com")
     (start-process "page-me" "*debug*" "growlnotify.com" "/a:Emacs" "/n:IRC" msg))
    ((executable-find "growlnotify")
     (start-process "page-me" "*debug*" "growlnotify" "-a" "Emacs" "-m" msg))
    ((executable-find "osascript")
     (apply 'start-process `("page-me" nil
			     "osascript"
			     "-e" "tell application \"GrowlHelperApp\""
			     "-e" "register as application \"Emacs\" all notifications {\"rcirc\"} default notifications {\"rcirc\"}"
			     "-e" ,(concat "notify with name \"rcirc\" title \"rcirc\" description \""
					   msg "\" application name \"Emacs\"")
			     "-e" "end tell")))
    (t (error "No method available to page you."))))

(defun rcirc-notify (sender &optional text)
  (when window-system
    ;; Set default dir to appease the notification gods
    (let ((default-directory "~/"))
      (rcirc-notify-page-me (format rcirc-notify-message sender text)))))

(defun rcirc-notify-keyword (sender &optional keyword text)
  (when window-system
    ;; Set default dir to appease the notification gods
    (let ((default-directory "~/"))
      (rcirc-notify-page-me (format rcirc-notify-keyword sender keyword text)))))

(defun rcirc-notify-private (sender &optional text)
  (when window-system
    ;; Set default dir to appease the notification gods
    (let ((default-directory "~/"))
      (rcirc-notify-page-me (format rcirc-notify-message-private sender text)))))

(defun rcirc-notify-allowed (nick &optional delay)
  "Return non-nil if a notification should be made for NICK.
If DELAY is specified, it will be the minimum time in seconds
that can occur between two notifications.  The default is
`rcirc-notify-timeout'."
  ;; Check current frame buffers
  (let ((rcirc-in-a-frame-p
         (some (lambda (f)
                 (and (equal "rcirc" (cdr f))
                      (car f)))
               (mapcar (lambda (f)
                         (let ((buffer (car (frame-parameter f 'buffer-list))))
                           (with-current-buffer buffer
                             (cons buffer mode-name))))
                       (visible-frame-list)))))
    (if (and rcirc-notify-check-frame (not rcirc-in-a-frame-p))
        (progn
          (unless delay (setq delay rcirc-notify-timeout))
          (let ((cur-time (float-time (current-time)))
                (cur-assoc (assoc nick rcirc-notify--nick-alist))
                (last-time))
            (if cur-assoc
                (progn
                  (setq last-time (cdr cur-assoc))
                  (setcdr cur-assoc cur-time)
                  (> (abs (- cur-time last-time)) delay))
              (push (cons nick cur-time) rcirc-notify--nick-alist)
              t))))))

###autoload
(defun rcirc-notify-me (proc sender response target text)
  "Notify the current user when someone sends a message that
matches the current nick."
  (interactive)
  (when (and (not (string= (rcirc-nick proc) sender))
	     (not (string= (rcirc-server-name proc) sender))
	     (rcirc-notify-allowed sender))
    (cond ((string-match (rcirc-nick proc) text)
	   (rcirc-notify sender text))
	  (rcirc-notify-keywords
	   (let ((keyword (catch 'match
			    (dolist (key rcirc-keywords)
			      (when (string-match (concat "\\<" key "\\>")
						  text)
				(throw 'match key))))))
	     (when keyword
	       (rcirc-notify-keyword sender keyword text)))))))

###autoload
(defun rcirc-notify-privmsg (proc sender response target text)
  "Notify the current user when someone sends a private message
to them."
  (interactive)
  (when (and (string= response "PRIVMSG")
             (not (string= sender (rcirc-nick proc)))
             (not (rcirc-channel-p target))
             (rcirc-notify-allowed sender))
    (rcirc-notify-private sender text)))

###autoload
(defun rcirc-notify-add-hooks ()
  "Initialize rcirc-notify into rcirc with hooks."
  (add-hook 'rcirc-print-hooks 'rcirc-notify-privmsg)
  (add-hook 'rcirc-print-hooks 'rcirc-notify-me)
  )

(provide 'rcirc-notify)

rcirc-notify.el ends here
