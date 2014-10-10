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

(eval-after-load 'rcirc '(require 'rcirc-notify))
