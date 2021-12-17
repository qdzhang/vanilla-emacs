;;; init.el -*- lexical-binding: t; -*-

;; Set path to dependencies
(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))

(setq config-dir
      (expand-file-name "config" user-emacs-directory))

;; Set up load path
(add-to-list 'load-path config-dir)
(add-to-list 'load-path site-lisp-dir)

(require 'cl-lib)
(require 'init-misc)
(require 'init-appearance)
(require 'init-parens)

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))
