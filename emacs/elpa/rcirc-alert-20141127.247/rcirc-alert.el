;;; rcirc-alert.el --- Configurable alert messages on top of RCIRC

;; Authors: Will Farrington, Alex Schroeder, Cayetano Santos
;; Maintainer: Cayetano Santos
;; Created: First December 2013
;; Version: 0.1
;; Keywords: lisp, rcirc, irc, alert, awesome

;; This file is NOT part of GNU Emacs.
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA
;;
;; Changelog:
;;
;; * 2013/12/01   First release. Personal (Cayetano Santos) review of the code found at
;;                Emacs Wiki at this date. See below for details.
;;
;; This code is a review of copy-paste of Emacs Wiki contributed original file called rcirc-notify.el
;;
;; Copyright (c) 2008 Will Farrington
;; Copyright (c) 2009, 2011 Alex Schroeder <alex@gnu.org>
;;
;; at http://www.emacswiki.org/emacs/rcirc-notify.el
;;
;;; Commentary:
;;
;; This code implements a notification (alert) library on top of emacs rcirc IRC client.
;;
;; This code integrates part of the original sources. It departs from the original in two ways
;;
;; - alerts are considered as a trigger action; any trigger action executes one script file (or anything else)
;;
;; - five different types of alerts are considered here, three of them may be disabled
;;
;;   "message" - someone talks to the user by classical "user_nick: ... " (always enabled)
;;   "keyword" - keyword detected
;;   "private" - private message (always enabled)
;;   "nick"    - given nicks changes status (join, ...)
;;   "always"  - alert of any message
;;
;; As long as this code only triggers an action, an example a bash script ('rcirc-alert.sh') is necessary
;; the alerts. This script will create a notification or whatever the user needs. It accepts four input
;; arguments, being the first argument the alert type. Refer to the example provided script file for more details.
;; This scripts provides an example configuration file showing a setup and how to use this library.
;;
;; Useful variables to customize alerts are described in the *Variables section of this file.
;;
;; ** Messages
;; ** Enables
;; ** Action script
;; ** Lists of alert triggers

;; These may be used in a per-buffer basis to specify individual needs on given chats (notify anything on twitter channel, alert when
;; given nick appears online at #emacs, keywords on #archlinux, etc.)

;;; TODO

;;; Code:


(require 'rcirc)
(require 'xfrp_find_replace_pairs)

;;; * GROUP

(defgroup rcirc-alert nil
  "Alerts thought a script for rcirc irc mode."
  :group 'rcirc)

;;; * Variables

;;; ** Alert Messages
;; Text to be displayed in the alert for each type of notification

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

;;; ** Enable / Disable Alerts
;; "message" and "private" are always enabled, the remaining may be disabled

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

;;; ** Action script

(defcustom my-rcirc-notification-script (concat user-emacs-directory "rcirc-alert.sh")
  "Script to trigger when a notification condition is met.
This script manages all types of notifications internally following its first argument."
  :type 'string
  :group 'rcirc-alert)

;;; ** Lists of alert triggers

(defcustom rcirc-alert-nicks nil
  "List of nicks whose new status triggers a notification of type 'nick'."
  :type '(repeat string)
  :group 'rcirc-alert)

(defcustom rcirc-alert-keywords nil
  "List of keywords that trigger a notification of type 'keyword'."
  :type '(repeat string)
  :group 'rcirc-alert)

;;; **

(defvar rcirc-alert-nick-alist nil
  "An alist of nicks and the last time they tried to trigger a notification.")

(defcustom rcirc-alert-timeout 60
  "Number of seconds that will elapse between notifications from the same person."
  :type 'integer
  :group 'rcirc-alert)


;;; * Notification Action
;; Comment

(defun RemovePattern (my-pattern str-chain)
  "Remove MY-PATTERN from STR-CHAIN.
This is useful when message content is poluted with links, pics and the like,
as is the case of twitter messages through bitlbee."
  (while (string-match-p my-pattern str-chain)
	(let* ((inicio (string-match-p my-pattern str-chain))
		   (fin (string-match-p " " (substring str-chain inicio nil))))
	  (setq str-chain (concat
					   (substring str-chain 0 (- inicio (if (< inicio 2) 0 1)))
					   (if fin
						   (substring str-chain (+ inicio fin) nil)
						 "" )))))
  (eval str-chain))

(defun asciify-text (ξstring &optional ξfrom ξto)
"Change some Unicode characters into equivalent ASCII ones. For example, “passé” becomes “passe”.
This function works on chars in European languages, and does not transcode arbitrary Unicode chars
(such as Greek, math symbols).
Un-transformed unicode char remains in the string. When called interactively, work on text selection or current block.
When called in lisp code, if ξfrom is nil, returns a changed string, else, change text in the region
between positions ξfrom ξto.
By Xah Lee http://ergoemacs.org/emacs/emacs_zap_gremlins.html"
  (interactive
   (if (region-active-p)
	   (list nil (region-beginning) (region-end))
	 (let ((bds (bounds-of-thing-at-point 'paragraph)) )
	   (list nil (car bds) (cdr bds)) ) ) )
  (let (workOnStringP
		inputStr
		(charChangeMap [
						["á\\|à\\|â\\|ä\\|ã\\|å" "a"]
						["é\\|è\\|ê\\|ë" "e"]
						["í\\|ì\\|î\\|ï" "i"]
						["ó\\|ò\\|ô\\|ö\\|õ\\|ø" "o"]
						["ú\\|ù\\|û\\|ü"     "u"]
						["Ý\\|ý\\|ÿ"     "y"]
						["ñ" "n"]
						["ç" "c"]
						["ð" "d"]
						["þ" "th"]
						["ß" "ss"]
						["æ" "ae"]
						]))
	(setq workOnStringP (if ξfrom nil t))
	(setq inputStr (if workOnStringP ξstring (buffer-substring-no-properties ξfrom ξto)))
	(if workOnStringP
		(let ((case-fold-search t)) (replace-regexp-pairs-in-string inputStr charChangeMap) )
	  (let ((case-fold-search t)) (replace-regexp-pairs-region ξfrom ξto charChangeMap) )) ) )

(defun my-page-me-notify (type title msg)
  "If notification script is in path, run it for this TYPE of notification."
  (cond
   ((executable-find my-rcirc-notification-script)
	(if rcirc-target
		;; process
		(start-process "page-me" nil my-rcirc-notification-script type title msg rcirc-target)))
   (t (error "No method available to page you."))))

;;; * Notificators
;; wrappers around the notification action

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
	(let ((default-directory "~/")
		  (msg (format rcirc-alert-message-always (upcase sender) text)))
	  (message " " )
	  (message (concat "Nuevo message : " msg) )
	  (setq msg (RemovePattern "\\[" msg))                                       ;; filter-out meta from message
	  ;; (setq msg (replace-regexp-in-string "\\[\\(.*\\)\\] " "" msg))          ;; filter-out meta from message
	  ;; (setq msg (replace-regexp-in-string "\\[.*\\] " "" msg))                ;; filter-out meta from message
	  (setq msg (replace-regexp-in-string " ?http.*://t.co/[-A-Za-z0-9]+" "" msg)) ;; filter-out links
	  (setq msg (replace-regexp-in-string " ?<\\(.*\\)>" "" msg))                ;; filter-out pics
	  (setq msg (asciify-text msg))                                              ;; convert message to ascii to avoid problems with naughty.notify
	  (setq msg (replace-regexp-in-string "\"" "'" msg))                         ;; convert double quotes
	  (setq msg (replace-regexp-in-string "¿" "" msg))                           ;; remove any spanish question mark
	  (message (concat "Fixed message : " msg) )                                 ;; echo fixed message
	  (message " " )
	  (my-page-me-notify "always" "rcIRC " msg))))

;;; * Allowed Senders
;; Comment

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


;;; * Criteria to trigger notifications

;;; ** notifications 1 and 2 : message and keyword
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

;;; ** notification 3 (private message)
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

;;; ** notification 4 (new nick status)
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

;;; ** notification 5 : notificate always
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


;;; * Hooks
;; When something gets printed, trigger an action

;; Message and keyword notification (1 & 2)
(add-hook 'rcirc-print-functions 'rcirc-alert-message)

;; Private message notification (3)
(add-hook 'rcirc-print-functions 'rcirc-alert-private)

;; Nick status change notification (4)
(add-hook 'rcirc-print-functions 'rcirc-alert-nick)

;; Always notification (5)
(add-hook 'rcirc-print-functions 'rcirc-alert-always)

(provide 'rcirc-alert)

;;; rcirc-alert.el ends here
