;; Keyboard preferences

(global-set-key "\C-w" 'backward-kill-word) ; this is faster than backspace
(global-set-key "\C-x\C-k" 'kill-region)    ; formerly bound to \C-w
(global-set-key "\C-c\C-k" 'kill-region)    ; formerly bound to \C-w

(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

(global-set-key (kbd "<f8> w") 'whitespace-mode)
(global-set-key (kbd "<f8> c") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "<f8> r") 'replace-string)
(global-set-key (kbd "<f8> R") 'replace-regexp)

(global-set-key (kbd "<f7> a") 'ack)

(if (boundp 'mac-command-modifier)
    (setq mac-command-modifier 'meta))
