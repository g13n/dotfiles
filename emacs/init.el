;; ---------------------------------------------------------------------------
;; Begin Emacs customization
;; ---------------------------------------------------------------------------

;; Package sources
(if (>= (string-to-number emacs-version) 24)
    (progn
      (require 'package)
      (add-to-list 'package-archives
		   '("melpa" . "http://melpa.milkbox.net/packages/") t)
      (package-initialize)))

(load-file (expand-file-name "~/.emacs.d/init/lib.el"))
(load-file (expand-file-name "~/.emacs.d/init/core.el"))
(load-file (expand-file-name "~/.emacs.d/init/ui.el"))
(load-file (expand-file-name "~/.emacs.d/init/kbd.el"))

;; Load custom vendor files
(load-directory "~/.emacs.d/init/vendor")

;; ... finally all programming language modes and tools
(load-file (expand-file-name "~/.emacs.d/init/dev.el"))

(load-file (expand-file-name "~/.emacs.d/init/srv.el"))

;; Load all custom files in sorted order
(load-directory "~/.emacs.d/init/custom")

;; ---------------------------------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3af98081c0cf235b7fba9ef7389dde9cfe79ab000fcb0e81735a55003258a7c5" "eea517c5cc867585ba1318d5bb9357b8a5b586e9220f32f35b15124b06d36029" "4e262566c3d57706c70e403d440146a5440de056dfaeb3062f004da1711d83fc" "d25594db11e666e3d670c739266f62b206814e261e8548becef5258d6cfbd50b" "e3a3b7d7fe89b5d57d40bc825ca2324875a6f37bd63da66f2a6fc68cc8b2ee95" "53e29ea3d0251198924328fd943d6ead860e9f47af8d22f0b764d11168455a8e" "0795e2c85394140788d72d34969be4acb305e4a54149e7237787d9df27832fbb" "53c542b560d232436e14619d058f81434d6bbcdc42e00a4db53d2667d841702e" "bd115791a5ac6058164193164fd1245ac9dc97207783eae036f0bfc9ad9670e0" "a99e7c91236b2aba4cd374080c73f390c55173c5a1b4ac662eeb3172b60a9814" "fc2782b33667eb932e4ffe9dac475f898bf7c656f8ba60e2276704fabb7fa63b" "3b819bba57a676edf6e4881bd38c777f96d1aa3b3b5bc21d8266fa5b0d0f1ebf" "60f04e478dedc16397353fb9f33f0d895ea3dab4f581307fbf0aa2f07e658a40" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "3c708b84612872e720796ea1b069cf3c8b3e909a2e1da04131f40e307605b7f9" "572caef0c27b100a404db8d540fd5b31397f90ab660ef5539ff0863ff9bee26a" default)))
 '(delete-selection-mode nil)
 '(mark-even-if-inactive t)
 '(scroll-bar-mode (quote right)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
