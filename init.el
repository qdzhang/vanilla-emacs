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

;; Determine the screen width
(defcustom my/screen-width (display-pixel-width)
  "The width of my screen.")

(defcustom my/screen-big-p nil
  "Whether the screen is bigger than 1920. If the width of screen is bigger than
1920, the value is t. Otherwise the value is nil (default).")

(when (> my/screen-width 1920)
  (setq my/screen-big-p t))

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

(require 'lazy-load)

(fido-vertical-mode)
;; Make `icomplete' related commands faster
(setq icomplete-delay-completions-threshold 2000)
(setq icomplete-compute-delay 0)

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

;; Autoload files
(require 'my-all-loaddefs)

;; Languages
(with-eval-after-load 'cc-mode
  (require 'init-c))
(with-eval-after-load 'lisp-mode
  (require 'init-lisp))
(require 'init-tempo)
(with-eval-after-load 'org
  (require 'init-org))
(require 'init-flyspell)
(with-eval-after-load 'sml-mode
  (require 'init-ml))
(with-eval-after-load 'cperl-mode
  (require 'init-perl))
(with-eval-after-load 'raku-mode
  (require 'init-perl))
(with-eval-after-load 'forth-mode
  (require 'init-forth))
(with-eval-after-load 'go-ts-mode
  (require 'init-go)
  (require 'init-eglot))

(require 'init-elisp)
(require 'init-python)
(with-eval-after-load 'python-mode
  (require 'init-eglot))


(with-eval-after-load 'flymake
  (require 'flymake-diagnostic-at-point)
  (require 'init-flymake)
  (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode))

;; Set up the required frontend for `company-mode', but not load it.
;; So this line will not slow down the startup time.
(add-hook 'after-init-hook 'company-tng-mode)

;; Some package can be loaded defered
(run-with-idle-timer
 0.5 nil
 #'(lambda ()
     (require 'init-edit)
     (require 'init-isearch)
     (require 'init-grep)
     (require 'init-navigate)
     (require 'init-fold)
     (require 'init-tags)
     (require 'init-treesit)
     (require 'init-transient)

     ;; (require 'init-company)
     (require 'init-completion)
     (require 'init-dired)
     (require 'init-docview)
     (require 'init-eshell)
     (require 'init-term)
     (require 'init-rime)
     (require 'init-defer-misc)

     (require 'init-parens)
     (require 'init-smartparens)

     (require 'init-tab-bar)

     ;; (when (> emacs-major-version 27)
     ;;   (require 'init-menu))

     (require 'init-hi-lock)
     (require 'init-proxy)
     (require 'init-auto-insert)

     (require 'init-web)
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


;; To disable collection of benchmark data after init is done.
(add-hook 'after-init-hook 'benchmark-init/deactivate)
