;;; init-defer-misc.el --- Some misc configurations can be deferred  -*- lexical-binding: t; -*-

;;; Commentary:

;; Defer loading some configurations

;;; Code:

(setq user-full-name "qdzhang"
      user-mail-address "qdzhangcn@gmail.com")

;; https://www.emacswiki.org/emacs/SavePlace
(save-place-mode 1)

;; https://www.emacswiki.org/emacs/SmoothScrolling
;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq scroll-conservatively 100000)
(setq scroll-preserve-screen-position 1)

;; pixel scroll after Emacs 29
(when (> emacs-major-version 28)
  (pixel-scroll-precision-mode 1)
  ;; Use `C-v' and `M-v' to pixel scroll, see `init-navigate' for details
  (setq pixel-scroll-precision-interpolate-page t))

;; Config how to construct unique buffer names for files with same base name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;;; Show tabs
(defun my/show-tabs ()
  (setq whitespace-style
        '(face
          ;; show tab as » (see `whitespace-display-mappings')
          tab-mark))
  (whitespace-mode 1))

;;; Show trailing whitespace
(defun my/show-trailing-whitespace ()
  (set-face-attribute 'trailing-whitespace nil
                      :background "green")
  (setq show-trailing-whitespace 1))
(add-hook 'prog-mode-hook 'my/show-trailing-whitespace)
(add-hook 'prog-mode-hook 'my/show-tabs)

(with-eval-after-load 're-builder
  ;; Support a slightly more idiomatic quit binding in re-builder
  (define-key reb-mode-map (kbd "C-c C-k") 'reb-quit))

(when (> emacs-major-version 27)
  (setq xref-search-program 'ripgrep))

;;; Enable the disabled functions
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region   'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'list-timers 'disabled nil)

;; Disable vc and use magit exclusively
;; (setq vc-handled-backends nil)

(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
(setq-default display-fill-column-indicator-column 80)
(setq-default fill-column 80)

(add-hook 'prog-mode-hook #'goto-address-prog-mode)

(setq browse-url-browser-function 'browse-url-firefox)

(global-so-long-mode 1)

;; Show context lines in `occur-mode'
(setq list-matching-lines-default-context-lines 1)

(defun notify-os (headline-string message-string)
  "Send message to notification"
  (shell-command (concat "notify-send --icon=emacs \""
                         headline-string
                         "\" \""
                         message-string
                         "\"")))

;; Setup buffers and windows
(setq display-buffer-alist
      '(
        ;; Split windows below
        ("\\*rg\\*" (display-buffer-reuse-mode-window display-buffer-below-selected))
        ("\\*Occur\\*" (display-buffer-reuse-mode-window display-buffer-below-selected))
        ("\\*reply\\*" (display-buffer-reuse-mode-window display-buffer-below-selected))

        ;; Avoid popup `Async Shell Command' window when using `dired-do-async-shell-command'
        ;; https://emacs.stackexchange.com/questions/5553/async-shell-process-buffer-always-clobbers-window-arrangement
        ("\\*Async Shell Command\\*.*" (display-buffer-no-window))))

;; Make comint prompt and output text readonly
(setq comint-prompt-read-only t)
(defun my/comint-preoutput-turn-buffer-read-only (text)
  (propertize text 'read-only t))

(add-hook 'comint-preoutput-filter-functions 'my/comint-preoutput-turn-buffer-read-only)

(provide 'init-defer-misc)
;;; init-defer-misc.el ends here
