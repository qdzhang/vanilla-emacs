;;; init-appearance.el -*- lexical-binding: t; -*-

(set-face-attribute 'default nil :font (font-spec :family "M+ 1mn" :size 24))
(set-face-attribute 'variable-pitch nil :family "IBM Plex Serif")
(set-face-attribute 'fixed-pitch nil :family "Sarasa Mono SC")
;; (set-fontset-font t 'han "LXGW WenKai")
(set-fontset-font t 'han "Sarasa Mono SC")
(set-fontset-font t 'kana "Sarasa Mono J")
(set-fontset-font t 'hangul "Sarasa Mono K")
(set-fontset-font t 'cjk-misc "Sarasa Mono SC")
(set-fontset-font t 'bopomofo "Sarasa Mono SC")

;; Color emoji: 😄, 🤦, 🏴󠁧󠁢󠁳󠁣󠁴󠁿
(set-fontset-font t 'symbol "Noto Color Emoji")
;; (set-fontset-font t 'symbol "Noto Sans Symbols" nil 'append)
;; (set-fontset-font t 'symbol "Noto Sans Symbols2" nil 'append)
(set-fontset-font t 'symbol "JetBrainsMono Nerd Font Mono" nil 'append)
(set-fontset-font t 'symbol "Symbola" nil 'append)


;; (load-theme 'tsdh-light t)


(setq modus-themes-no-mixed-fonts t
      modus-themes-org-blocks 'gray-background)

(if (display-graphic-p)
    (load-theme 'modus-operandi)
  (load-theme 'modus-vivendi))

(define-key global-map (kbd "<f6>") #'modus-themes-toggle)

(provide 'init-appearance)
