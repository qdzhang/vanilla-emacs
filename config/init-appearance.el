;;; init-appearance.el -*- lexical-binding: t; -*-

;; * Default fonts settings
;; (set-face-attribute 'default nil :font (font-spec :family "M+ 1mn" :size 24))
(set-face-attribute 'default nil :font (font-spec :family "Latin Modern Mono" :size 26))
;; (set-face-attribute 'default nil :font (font-spec :family "Input Mono Condensed" :size 24))
;; (set-face-attribute 'default nil :font (font-spec :family "JetbrainsMono Nerd Font Mono" :size 24))
;; (set-face-attribute 'variable-pitch nil :family "IBM Plex Serif")
;; (set-face-attribute 'variable-pitch nil :family "Vollkorn")
(set-face-attribute 'variable-pitch nil :family "Libertinus Serif")
;; (set-face-attribute 'variable-pitch nil :family "Latin Modern Roman")
;; (set-face-attribute 'variable-pitch nil :family "Liberation Serif")
;; (set-face-attribute 'variable-pitch nil :family "Input Serif Compressed")
(set-face-attribute 'fixed-pitch nil :family "Sarasa Mono SC")
;; (set-face-attribute 'fixed-pitch nil :family "M+ 1mn")
(set-face-attribute 'fixed-pitch-serif nil :family "Latin Modern Mono")

(defface my/font-org-header
  '()
  "The font face used for org headers"
  :group 'basic-faces)
;; Libertinus Sans is also good font for headers
(set-face-attribute 'my/font-org-header nil :family "Sarasa Fixed Slab SC")

(set-fontset-font t 'han "LXGW WenKai")
;; (set-fontset-font t 'han "Sarasa Mono SC")
(set-fontset-font t 'kana "Sarasa Mono J")
(set-fontset-font t 'hangul "Sarasa Mono K")
(set-fontset-font t 'cjk-misc "Sarasa Mono SC")
(set-fontset-font t 'bopomofo "Sarasa Mono SC")
(set-fontset-font t 'greek (font-spec :family "Helvetica"))
(set-fontset-font t 'phonetic "Noto Sans")


;; Color emoji: 😄, 🤦, 🏴󠁧󠁢󠁳󠁣󠁴󠁿
(unless (version< emacs-version "28.1")
  (set-fontset-font t 'emoji "Noto Color Emoji"))
(set-fontset-font t 'symbol "Noto Color Emoji")
;; (set-fontset-font t 'symbol "Noto Sans Symbols" nil 'append)
;; (set-fontset-font t 'symbol "Noto Sans Symbols2" nil 'append)
(set-fontset-font t 'symbol "JetBrainsMono Nerd Font Mono" nil 'append)
(set-fontset-font t 'symbol "Symbola" nil 'append)
(set-fontset-font t nil "98WB-U" nil 'prepend)

;; Scale font
;;
;; Make the font bigger but not effect the font size in some scenarios, such as
;; table in org-mode.
(dolist (mode '(prog-mode-hook text-mode-hook sdcv-mode-hook))
  (add-hook mode (lambda () (text-scale-increase 1.2))))


;; * Themes settings
;; (load-theme 'tsdh-light t)


;; (setq modus-themes-no-mixed-fonts t
;;       modus-themes-org-blocks 'gray-background)

;; (if (display-graphic-p)
;;     (load-theme 'modus-operandi)
;;   (load-theme 'modus-vivendi))

;; (define-key global-map (kbd "<f6>") #'modus-themes-toggle)

;; ** Modify built-in `misterioso' theme
;; (load-theme 'misterioso)
;; (custom-theme-set-faces
;;  'misterioso
;;  '(cursor ((t (:background "#abb2bf"))))
;;  '(one-key-keystroke ((t (:foreground "#f29112")))))
;; (enable-theme 'misterioso)


;; ** Modify default light theme

;; Current color theme. Acceptable values: 'light, 'dark
;; Default is light
(defvar my/current-color-theme 'light)
(add-hook 'after-init-hook 'my/light-theme)
(define-key global-map (kbd "<f7>") #'my/theme-toggle-color)

(defun my/theme-toggle-color ()
  "Toggle between light and dark color scheme."
  (interactive)
  (cond ((eq my/current-color-theme 'light)
         (my/dark-theme))
        ((eq my/current-color-theme 'dark)
         (my/light-theme))))

;; Default light theme with some customization
(defun my/light-theme ()
  (interactive)
  (setq my/current-color-theme 'light)
  (custom-theme-set-faces
   'user
   `(default ((t (:background "#fcf7e8" :foreground "black"))))
   `(cursor ((t (:background "black"))))
   `(font-lock-comment-face ((t :foreground "Firebrick")))
   ;; Make `term' and `ansi-term' prompt more distinguishable
   `(ansi-color-white ((t :background "gray65" :foreground "gray65")))
   `(region ((t :background "#edccb7")))
   `(mode-line ((t (
                    :inherit variable-pitch
                    :height 1.1
                    :box (:line-width (-1 . -1) :color "#a8a296")
                    :background "#e5dac4"))))
   `(mode-line-inactive ((t (
                             :inherit variable-pitch
                             :height 1.1
                             :box (:line-width (-1 . -1) :color "#a8a296")
                             :background "#f8f2e0"))))))

(defun my/dark-theme ()
  (interactive)
  (setq my/current-color-theme 'dark)
  (custom-theme-set-faces
   'user
   `(default ((t :background "#1a1a1a" :foreground "#e0e0e0")))
   `(cursor ((t :background "#e0e0e0")))
   `(font-lock-comment-face ((t :foreground "violet")))
   `(ansi-color-white ((t :background "gray90" :foreground "gray90")))
   `(region ((t :background "#342464")))
   `(mode-line ((t (
                    :inherit variable-pitch
                    :height 1.1
                    :box (:line-width (-1 . -1) :color "dim gray")
                    :background "#323232"))))
   `(mode-line-inactive ((t (
                             :inherit variable-pitch
                             :height 1.1
                             :box (:line-width (-1 . -1) :color "dim gray")
                             :background "#222222"))))))

(with-eval-after-load 'easy-kill
  (set-face-attribute 'easy-kill-selection nil :inherit 'highlight))

;; Highlight TODO watchwords
(defun my/add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIXME\\|TODO\\|FIX\\)"
          1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook 'my/add-watchwords)


;; ** Modify built-in `wombat' theme
;; (load-theme 'wombat)
;; (custom-theme-set-faces
;;  'wombat
;;  '(cursor ((t (:background "#abb2bf"))))
;;  '(one-key-keystroke ((t (:foreground "#f29112")))))

;; Apply the preceding custom theme faces immediately
;; https://emacs.stackexchange.com/a/60628
;; (enable-theme 'wombat)

(defun my/switch-theme (theme)
  ;; This interactive call is taken from `load-theme'
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapcar 'symbol-name
                                     (custom-available-themes))))))
  (mapcar #'disable-theme custom-enabled-themes)
  (load-theme theme t))

(provide 'init-appearance)
