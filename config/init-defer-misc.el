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
;;
;; To make ansi-term obey the `display-buffer-alist'
;; https://emacs.stackexchange.com/questions/48472/how-to-make-m-x-ansi-term-behave-like-m-x-shell-opening-in-new-window/48481#48481
;; https://emacs.stackexchange.com/questions/81033/run-ansi-term-in-a-new-frame
;; https://old.reddit.com/r/emacs/comments/novb5o/displaybufferalist_doesnt_work_with_ansiterm_and/
(setq switch-to-buffer-obey-display-actions t)

(setq display-buffer-alist
      '(
        ;; Split windows below
        ("\\*rg\\*" (display-buffer-reuse-mode-window display-buffer-below-selected))
        ("\\*Occur\\*" (display-buffer-reuse-mode-window display-buffer-below-selected))
        ("\\*reply\\*" (display-buffer-reuse-mode-window display-buffer-below-selected))

        ;; Avoid popup `Async Shell Command' window when using `dired-do-async-shell-command'
        ;; https://emacs.stackexchange.com/questions/5553/async-shell-process-buffer-always-clobbers-window-arrangement
        ("\\*Async Shell Command\\*.*" (display-buffer-no-window))))

(cl-defun my/display-buffer-in-side-window (&optional (buffer (current-buffer)))
  "Display BUFFER in dedicated side window.
Ref: https://gist.github.com/alphapapa/c5458365e9940069e6a52a2a95b1ccff"
  (interactive)
  (let ((display-buffer-mark-dedicated t))
    (display-buffer-in-side-window buffer
                                   '((side . bottom)
                                     (slot . -1)
                                     (window-height . 0.33)
                                     (window-parameters
                                      (no-delete-other-windows . t))))))


;; Make comint prompt and output text readonly
(setq comint-prompt-read-only t)
(defun my/comint-preoutput-turn-buffer-read-only (text)
  (propertize text 'read-only t))

(add-hook 'comint-preoutput-filter-functions 'my/comint-preoutput-turn-buffer-read-only)

;; Load the wakatime api and config.
(require 'init-waka)
(global-wakatime-mode)

;; Load direnv config
(require 'init-direnv)

(provide 'init-defer-misc)
;;; init-defer-misc.el ends here
