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

  (add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
  (setq-default display-fill-column-indicator-column 80)
  (setq-default fill-column 80)

  ;; Fix CJK word wrap
  ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=29364
  (setq-default word-wrap-by-category t)

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

(put 'narrow-to-region 'disabled nil)

;;; When use FIND-FILE to create a new file, if the directory doesn't exist yet,
;;; create the directory
;;; https://superuser.com/questions/131538/can-i-create-directories-that-dont-exist-while-creating-a-new-file-in-emacs
(defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (make-directory dir t)))))

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

(provide 'init-misc)
