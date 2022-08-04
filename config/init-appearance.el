;;; init-appearance.el -*- lexical-binding: t; -*-

;; (set-face-attribute 'default nil :font (font-spec :family "M+ 1mn" :size 24))
(set-face-attribute 'default nil :font (font-spec :family "Input Mono Condensed" :size 24))
;; (set-face-attribute 'variable-pitch nil :family "IBM Plex Serif")
;; (set-face-attribute 'variable-pitch nil :family "Vollkorn Medium")
;; (set-face-attribute 'variable-pitch nil :family "Bookerly")
;; (set-face-attribute 'variable-pitch nil :family "Liberation Serif")
(set-face-attribute 'variable-pitch nil :family "Input Serif Compressed")
(set-face-attribute 'fixed-pitch nil :family "Sarasa Mono SC")
;; (set-fontset-font t 'han "LXGW WenKai")
(set-fontset-font t 'han "Sarasa Mono SC")
(set-fontset-font t 'kana "Sarasa Mono J")
(set-fontset-font t 'hangul "Sarasa Mono K")
(set-fontset-font t 'cjk-misc "Sarasa Mono SC")
(set-fontset-font t 'bopomofo "Sarasa Mono SC")
(set-fontset-font t 'greek (font-spec :family "Helvetica"))


;; Color emoji: 😄, 🤦, 🏴󠁧󠁢󠁳󠁣󠁴󠁿
(set-fontset-font t 'symbol "Noto Color Emoji")
;; (set-fontset-font t 'symbol "Noto Sans Symbols" nil 'append)
;; (set-fontset-font t 'symbol "Noto Sans Symbols2" nil 'append)
(set-fontset-font t 'symbol "JetBrainsMono Nerd Font Mono" nil 'append)
(set-fontset-font t 'symbol "Symbola" nil 'append)


;; (load-theme 'tsdh-light t)


(setq modus-themes-no-mixed-fonts t
      modus-themes-org-blocks 'gray-background)

;; (if (display-graphic-p)
;;     (load-theme 'modus-operandi)
;;   (load-theme 'modus-vivendi))

;; (define-key global-map (kbd "<f6>") #'modus-themes-toggle)

;; {{{ Modify built-in `misterioso' theme
;; (load-theme 'misterioso)
;; (custom-theme-set-faces
;;  'misterioso
;;  '(cursor ((t (:background "#abb2bf"))))
;;  '(one-key-keystroke ((t (:foreground "#f29112")))))
;; (enable-theme 'misterioso)
;; }}}

;; {{{ Modify default light theme
;; Default light theme
(set-background-color "white")
(set-foreground-color "black")
;; initial window settings
(setq initial-frame-alist
      '((background-color . "honeydew")))

;; subsequent window settings
(setq default-frame-alist
      '((background-color . "honeydew")))

(set-face-attribute 'region nil :background "#e9dd76")
(with-eval-after-load 'easy-kill
  (set-face-attribute 'easy-kill-selection nil :inherit 'highlight))
;;}}}

;; {{{ Modify built-in `wombat' theme
;; (load-theme 'wombat)
;; (custom-theme-set-faces
;;  'wombat
;;  '(cursor ((t (:background "#abb2bf"))))
;;  '(one-key-keystroke ((t (:foreground "#f29112")))))

;; Apply the preceding custom theme faces immediately
;; https://emacs.stackexchange.com/a/60628
;; (enable-theme 'wombat)
;; }}}


(provide 'init-appearance)
