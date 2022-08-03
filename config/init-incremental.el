;;; init-incremental.el --- Configure incremental loading packages  -*- lexical-binding: t; -*-

;;; Commentary:

;; Incrementally load some packages
;; Use the method extract from Doom Emacs
;; Ref: https://emacs-china.org/t/doom-emacs-emacs/21799

;;; Code:

(defvar my-defer/incremental-packages '(t)
  "A list of packages to load incrementally after startup. Any large packages
  here may cause noticeable pauses, so it's recommended you break them up into
  sub-packages. For example, `org' is comprised of many packages, and can be
  broken up into:

    (my-defer/load-packages-incrementally
     '(calendar find-func format-spec org-macs org-compat
       org-faces org-entities org-list org-pcomplete org-src
       org-footnote org-macro ob org org-clock org-agenda
       org-capture))

  This is already done by the lang/org module, however.

  If you want to disable incremental loading altogether, either remove
  `doom-load-packages-incrementally-h' from `emacs-startup-hook' or set
  `doom-incremental-first-idle-timer' to nil. Incremental loading does not occur
  in daemon sessions (they are loaded immediately at startup).")

(defvar my-defer/incremental-first-idle-timer 5.0
  "How long (in idle seconds) until incremental loading starts.

 Set this to nil to disable incremental loading.")

(defvar my-defer/incremental-idle-timer 0.75
  "How long (in idle seconds) in between incrementally loading packages.")

(defvar my-defer/incremental-load-immediately (daemonp)
  "If non-nil, load all incrementally deferred packages immediately at startup.")

(defun my-defer/load-packages-incrementally (packages &optional now)
  "Registers PACKAGES to be loaded incrementally.

  If NOW is non-nil, load PACKAGES incrementally, in `doom-incremental-idle-timer'
  intervals."
  (if (not now)
      (setq my-defer/incremental-packages (append my-defer/incremental-packages packages ))
    (while packages
      (let* ((gc-cons-threshold most-positive-fixnum)
             (req (pop packages)))
        (unless (featurep req)
          (message "Incrementally loading %s" req)
          (condition-case-unless-debug e
              (or (while-no-input
                    ;; If `default-directory' is a directory that doesn't exist
                    ;; or is unreadable, Emacs throws up file-missing errors, so
                    ;; we set it to a directory we know exists and is readable.
                    (let ((default-directory user-emacs-directory)
                          (inhibit-message t)
                          file-name-handler-alist)
                      (require req nil t))
                    t)
                  (push req packages))
            (error
             (message "Failed to load %S package incrementally, because: %s"
                      req e)))
          (if (not packages)
              (message "Finished incremental loading")
            (run-with-idle-timer my-defer/incremental-idle-timer
                                 nil #'my-defer/load-packages-incrementally
                                 packages t)
            (setq packages nil)))))))

(defun my-defer/load-packages-incrementally-h ()
  "Begin incrementally loading packages in `my-defer/incremental-packages'.

If this is a daemon session, load them all immediately instead."
  (if my-defer/incremental-load-immediately
      (mapc #'require (cdr my-defer/incremental-packages))
    (when (numberp my-defer/incremental-first-idle-timer)
      (run-with-idle-timer my-defer/incremental-first-idle-timer
                           nil #'my-defer/load-packages-incrementally
                           (cdr my-defer/incremental-packages) t))))

(my-defer/load-packages-incrementally
 '(calendar find-func format-spec org-macs org-compat
            org-faces org-entities org-list org-pcomplete org-src
            org-footnote org-macro ob org org-clock org-agenda
            org-capture magit bookmark
            eshell em-alias em-banner em-basic em-cmpl em-glob em-hist em-ls
            em-prompt em-script em-term em-unix))

(add-hook 'emacs-startup-hook #'my-defer/load-packages-incrementally-h)

(provide 'init-incremental)
;;; init-incremental.el ends here
