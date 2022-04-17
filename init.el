;;; init.el -*- lexical-binding: t; -*-

;;; Early birds
(progn
  (defvar before-user-init-time (current-time)
    "Value of `current-time' when Emacs begins loading `user-init-file'.")
  (message "Loading Emacs...done (%.3fs)"
           (float-time (time-subtract before-user-init-time
                                      before-init-time)))
  (message "Loading %s..." user-init-file)
  (when (< emacs-major-version 27)
    (setq package-enable-at-startup nil)
    (load-file (expand-file-name "early-init.el" user-emacs-directory))))

(eval-and-compile ; `borg'
  (add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
  (require 'borg)
  (borg-initialize))



;; Set path to dependencies
(setq config-dir
      (expand-file-name "config" user-emacs-directory))

;; Set up load path
(add-to-list 'load-path config-dir)

;; Basic packages
(require 'cl-lib)
(require 'one-key)
(require 'lazy-load)

(fido-vertical-mode)

;;; Config files
(require 'init-misc)
(require 'init-appearance)
(require 'init-parens)
(require 'init-smartparens)
(require 'init-aggressive-indent)
(require 'init-edit)
(require 'init-lazy-keys)

(progn
  (message "Loading %s...done (%.3fs)" user-init-file
           (float-time (time-subtract (current-time)
                                      before-user-init-time)))
  (add-hook 'after-init-hook
            (lambda ()
              (message
               "Loading %s...done (%.3fs) [after-init]" user-init-file
               (float-time (time-subtract (current-time)
                                          before-user-init-time))))
            t))

(progn
  (let ((file (expand-file-name (concat (user-real-login-name) ".el")
                                user-emacs-directory)))
    (when (file-exists-p file)
      (load file))))
