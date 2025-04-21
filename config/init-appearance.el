;;; init-appearance.el -*- lexical-binding: t; -*-

;; * Default fonts settings
;; (set-face-attribute 'default nil :font (font-spec :family "M+ 1mn" :size 24))
;; (set-face-attribute 'default nil :font (font-spec :family "Latin Modern Mono" :size 26))
;; (set-face-attribute 'default nil :font (font-spec :family "Inconsolata" :size 28))
;; (set-face-attribute 'default nil :font (font-spec :family "Noto Sans Mono" :size 26))
;; (set-face-attribute 'default nil :font (font-spec :family "IntelOne Mono" :size 26))
;; (set-face-attribute 'default nil :font (font-spec :family "Go Mono" :size 26))

;; Using Sarasa Font will slow down Emacs startup, use Iosevka instead.
;; (set-face-attribute 'default nil :font (font-spec :family "Sarasa Fixed SC" :size 26))
(set-face-attribute 'default nil :font (font-spec :family "Iosevka Fixed SS06" :size 26))


;; (set-face-attribute 'default nil :font (font-spec :family "Bedstead" :size 28))
;; (set-face-attribute 'default nil :font (font-spec :family "CozetteHiDpi"))
;; (set-face-attribute 'default nil :font (font-spec :family "Input Mono Condensed" :size 24))
;; (set-face-attribute 'default nil :font (font-spec :family "JetbrainsMono Nerd Font Mono" :size 24))
;; (set-face-attribute 'variable-pitch nil :family "IBM Plex Serif")
;; (set-face-attribute 'variable-pitch nil :family "Vollkorn")
(set-face-attribute 'variable-pitch nil :family "Libertinus Serif")
;; (set-face-attribute 'variable-pitch nil :family "Latin Modern Roman")
;; (set-face-attribute 'variable-pitch nil :family "Liberation Serif")
;; (set-face-attribute 'variable-pitch nil :family "Input Serif Compressed")
;; (set-face-attribute 'fixed-pitch nil :family "Noto Sans Mono CJK SC")
(set-face-attribute 'fixed-pitch nil :family "Sarasa Mono SC")
;; (set-face-attribute 'fixed-pitch nil :family "Inconsolata")
;; (set-face-attribute 'fixed-pitch nil :family "M+ 1mn")
(set-face-attribute 'fixed-pitch-serif nil :family "Latin Modern Mono")


;; Sarasa font set for CJK font
;; (set-fontset-font t 'han "Sarasa Mono SC")
;; 楷体
;; (set-fontset-font t 'han "FZKai-Z03")
;; 仿宋
;; (set-fontset-font t 'han "FZYouSong GBK")
;; 上图东观体
;; https://www.library.sh.cn/special/dongguanti/
(set-fontset-font t 'han "上图东观体 常规")
;; 书宋
;; (set-fontset-font t 'han "FZShuSong-Z01")

;; 使用 Noto Serif CJK JP 行高比一般的英文字体高，所以使用“一點明體”，可以和
;; 中文的宋体和英文的 serif 字形保持一致的视觉效果，还能保持行高一致。
;; (set-fontset-font t 'kana "Sarasa Mono J")
(set-fontset-font t 'kana "一點明體")
(set-fontset-font t 'hangul "Sarasa Mono K")
(set-fontset-font t 'cjk-misc "Sarasa Mono SC")
(set-fontset-font t 'bopomofo "Sarasa Mono SC")

;; Noto font set for CJK font
;; (set-fontset-font t 'han "Noto Sans Mono CJK SC")
;; (set-fontset-font t 'kana "Noto Sans Mono CJK JP")
;; (set-fontset-font t 'hangul "Noto Sans Mono CJK KR")
;; (set-fontset-font t 'cjk-misc "Noto Sans Mono CJK SC")
;; (set-fontset-font t 'bopomofo "Noto Sans Mono CJK SC")

;; Noto Serif font set for CJK font
;; (set-fontset-font t 'han "Noto Serif CJK SC")
;; (set-fontset-font t 'kana "Noto Serif CJK JP")
;; (set-fontset-font t 'hangul "Noto Serif CJK KR")
;; (set-fontset-font t 'cjk-misc "Noto Serif CJK SC")
;; (set-fontset-font t 'bopomofo "Noto Serif CJK SC")

;; Greek and phonetic font
(set-fontset-font t 'greek (font-spec :family "Helvetica"))
(set-fontset-font t 'phonetic "Noto Sans")


;; Color emoji: 😄, 🤦, 🏴󠁧󠁢󠁳󠁣󠁴󠁿
(unless (version< emacs-version "28.1")
  (set-fontset-font t 'emoji "Noto Color Emoji"))
(set-fontset-font t 'symbol "Noto Color Emoji")
(set-fontset-font t 'symbol "Noto Sans Symbols" nil 'append)
;; (set-fontset-font t 'symbol "Noto Sans Symbols2" nil 'append)
;; (set-fontset-font t 'symbol "JetBrainsMono Nerd Font Mono" nil 'append)
(set-fontset-font t 'symbol "Symbola" nil 'append)
(set-fontset-font t nil "98WB-U" nil 'prepend)

;; Scale font
;;
;; Make the font bigger but not effect the font size in some scenarios, such as
;; table in org-mode.
;;
;; Some hooks complement Latin Modern Mono:
;; - prog-mode-hook
;; - help-mode-hook
;; - conf-mode-hook
(dolist (mode '(org-mode-hook inf-sdcv-mode-hook markdown-mode-hook))
  (add-hook mode (lambda () (text-scale-increase 1.2))))

;; For a bigger external monitor
(when my/screen-big-p
  (add-hook 'emacs-startup-hook (lambda () (global-text-scale-adjust 8))))

;; * Themes settings
;; (load-theme 'tsdh-light t)
;; (load-theme 'tango t)


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
;; Setup a pair of dark/light themes
(defvar my/light-theme 'mylight)
(defvar my/dark-theme 'mydark)

(define-key global-map (kbd "<f7>") #'my/theme-toggle-color)

(add-to-list 'custom-theme-load-path
             (file-name-as-directory (concat user-emacs-directory "themes/")))

(defun my/theme-apply (theme)
  "Load and enable specific THEME."
  (load-theme theme t t)
  (enable-theme theme))

(cond ((eq my/current-color-theme 'light)
       (my/theme-apply my/light-theme))
      ((eq my/current-color-theme 'dark)
       (my/theme-apply my/dark-theme)))


(defun my/change-to-dark-theme ()
  "Change to a dark theme"
  (interactive)
  (setq my/current-color-theme 'dark)
  (mapcar #'disable-theme custom-enabled-themes)
  (my/theme-apply my/dark-theme))

(defun my/change-to-light-theme ()
  "Change to a light theme"
  (interactive)
  (setq my/current-color-theme 'light)
  (mapcar #'disable-theme custom-enabled-themes)
  (my/theme-apply my/light-theme))


(defun my/theme-toggle-color ()
  "Toggle between light and dark color scheme."
  (interactive)
  (cond ((eq my/current-color-theme 'light)
         (my/change-to-dark-theme))
        ((eq my/current-color-theme 'dark)
         (my/change-to-light-theme))))


;; Highlight TODO watchwords
(defun my/add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIXME\\|TODO\\)"
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
