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


(require 'benchmark-init)
(benchmark-init/activate)

;; Set path to dependencies
(setq config-dir
      (expand-file-name "config" user-emacs-directory))
(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))

;; Set up load path
(add-to-list 'load-path config-dir)
(add-to-list 'load-path site-lisp-dir)

;; Basic packages
(require 'cl-lib)
(require 'transient)
(require 'lazy-load)

(fido-vertical-mode)
(repeat-mode)

;;; Config files
(require 'init-misc)
(require 'init-appearance)
(require 'init-aggressive-indent)
(require 'init-mode)
(require 'init-lazy-keys)
(require 'init-super-save)
(require 'init-modeline)
(require 'init-incremental)

;; Some package can be loaded defered
(run-with-idle-timer
 1 nil
 #'(lambda ()
     (require 'init-edit)
     (require 'init-isearch)
     (require 'init-window)
     (require 'init-navigate)

     (require 'init-company)
     ;; (require 'init-completion)
     (require 'init-dired)
     (require 'init-docview)
     (require 'init-eshell)
     (require 'init-rime)
     (require 'init-defer-misc)

     (require 'init-parens)
     (require 'init-smartparens)

     (require 'init-project)
     (require 'init-tab-bar)

     (when (> emacs-major-version 27)
       (require 'init-menu))

     ;; Languages
     ;; (require 'init-c)
     (require 'init-lisp)
     (require 'init-tempo)
     (require 'init-org)
     (require 'init-flyspell)
     (require 'init-ml)
     (require 'init-perl)
     (require 'init-forth)

     (require 'init-hi-lock)
     (require 'init-proxy)
     (message "Deferred config loading...Done.")))

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


;; Show garbage collector's message.
;; Execute garbage collector when idle for 15 seconds.
(defmacro k-time (&rest body)
  "Measure and return the time it takes evaluating BODY."
  `(let ((time (current-time)))
     ,@body
     (float-time (time-since time))))

(defvar k-gc-timer
  (run-with-idle-timer 15 t
                       (lambda ()
                         (message "Garbage Collector has run for %.06fsec"
                                  (k-time (garbage-collect))))))


;; To disable collection of benchmark data after init is done.
(add-hook 'after-init-hook 'benchmark-init/deactivate)
