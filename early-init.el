;;; early-init.el --- earliest birds               -*- lexical-binding: t -*-

(setq load-prefer-newer t)

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "lib/packed" dir))
  (add-to-list 'load-path (expand-file-name "lib/auto-compile" dir)))
;(require 'auto-compile)
;(auto-compile-on-load-mode)
;(auto-compile-on-save-mode)

(setq package-enable-at-startup nil)

(setq inhibit-splash-screen 1)
(unless (memq window-system '(mac ns))
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

(if (boundp 'use-short-answers)
    ;; The variable was introduced in version 28.1
    (setq use-short-answers t)
  (fset 'yes-or-no-p 'y-or-n-p))

(setq initial-major-mode 'fundamental-mode)
(setq initial-scratch-message "\
# This buffer is for notes you don't want to save.
# If you want to create a file, visit that file with C-x C-f,
# then enter the text in that file's own buffer.

")


;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t)

;; Speed up startup
;;=================

;; Minimize garbage collection during startup
(setq gc-cons-threshold (expt 2 24)) ; 16mb

(defun defer-garbage-collection-h ()
  (setq gc-cons-threshold (expt 2 24)))

(defun restore-garbage-collection-h ()
  ;; Defer it so that commands launched immediately after will enjoy the
  ;; benefits.
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold (expt 2 23)))))

;; Lower threshold back to 8 MiB (default is 800kB)
(add-hook 'emacs-startup-hook #'restore-garbage-collection-h)
(add-hook 'minibuffer-setup-hook #'defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'restore-garbage-collection-h)

;; Increase the amount of data which Emacs reads from the
;; process. Again the emacs default is too low 4k considering that the
;; some of the language server responses are in 800k - 3M range.
(setq read-process-output-max (expt 2 24)) ;; 16mb


;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:
;;; early-init.el ends here
