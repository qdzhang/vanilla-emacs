;;; init-misc.el -*- lexical-binding: t; -*-

(progn
  (setq inhibit-splash-screen 1)
  (unless (memq window-system '(mac ns))
    (menu-bar-mode -1))
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
  (when (fboundp 'horizontal-scroll-bar-mode)
    (horizontal-scroll-bar-mode -1))

  (autoload 'zap-up-to-char "misc"
    "Kill up to, but not including ARGth occurrence of CHAR." t)

  ;; Config how to construct unique buffer names for files with same base name
  (require 'uniquify)
  (setq uniquify-buffer-name-style 'forward)

  ;; https://www.emacswiki.org/emacs/SavePlace
  (save-place-mode 1)

  ;; https://www.emacswiki.org/emacs/SmoothScrolling
  ;; scroll one line at a time (less "jumpy" than defaults)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
  (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
  (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
  (setq scroll-step 1) ;; keyboard scroll one line at a time

  (setq-default indent-tabs-mode nil)
  (savehist-mode 1)
  (setq save-interprogram-paste-before-kill t
        apropos-do-all t
        mouse-yank-at-point t
        require-final-newline t
        load-prefer-newer t
        ediff-window-setup-function 'ediff-setup-windows-plain
        custom-file (expand-file-name "~/.emacs.d/custom.el"))

  (unless backup-directory-alist
    (setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                                   "backups"))))))
(provide 'init-misc)
