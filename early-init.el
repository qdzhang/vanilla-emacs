;;; early-init.el --- earliest birds               -*- lexical-binding: t -*-

(setq load-prefer-newer t)

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

;; Some startup optimizations from Doom Emacs
;; https://github.com/doomemacs/doomemacs/blob/ef9e8d892556595fcdfd723d145c29593a67c50d/lisp/doom.el#L407
(unless (daemonp)
  ;; PERF: `file-name-handler-alist' is consulted on each call to `require',
  ;;   `load', or various file/io functions (like `expand-file-name' or
  ;;   `file-remote-p'). You get a noteable boost to startup time by unsetting
  ;;   or simplifying its value.
  (let ((old-value (default-toplevel-value 'file-name-handler-alist)))
    (set-default-toplevel-value
     'file-name-handler-alist
     ;; HACK: The elisp libraries bundled with Emacs are either compressed or
     ;;   not, never both. So if calc-loaddefs.el.gz exists, calc-loaddefs.el
     ;;   won't, and vice versa. This heuristic is used to guess the state of
     ;;   all other built-in (or site); if they're compressed, we must leave the
     ;;   gzip file handler in `file-name-handler-alist' so Emacs knows how to
     ;;   load them. Otherwise, we can omit it (at least during startup) for a
     ;;   boost in package load time.
     (if (eval-when-compile
           (locate-file-internal "calc-loaddefs.el" load-path))
         nil
       (list (rassq 'jka-compr-handler old-value))))
    ;; Remember it so it can be reset where needed.
    (put 'file-name-handler-alist 'initial-value old-value)
    ;; COMPAT: Eventually, Emacs will process any files passed to it via the
    ;;   command line, and will do so *really* early in the startup process.
    ;;   These might contain special file paths like TRAMP paths, so restore
    ;;   `file-name-handler-alist' just for this portion of startup.
    (define-advice command-line-1 (:around (fn args-left) respect-file-handlers)
      (let ((file-name-handler-alist
             (if args-left old-value file-name-handler-alist)))
        (funcall fn args-left)))
    ;; COMPAT: ...but restore `file-name-handler-alist' later, because it is
    ;;   needed for handling encrypted or compressed files, among other things.
    (add-hook 'emacs-startup-hook
              (lambda ()
                (set-default-toplevel-value
                 'file-name-handler-alist
                 ;; Merge instead of overwrite because there may have been
                 ;; changes to `file-name-handler-alist' since startup we want
                 ;; to preserve.
                 (delete-dups (append file-name-handler-alist old-value))))))

  (unless noninteractive
    (setq inhibit-startup-echo-area-message user-login-name)
    ;; PERF,UX: Prevent "For information about GNU Emacs..." line in *Messages*.
    (advice-add #'display-startup-echo-area-message :override #'ignore)
    ;; PERF: Suppress the vanilla startup screen completely. We've disabled it
    ;;   with `inhibit-startup-screen', but it would still initialize anyway.
    ;;   This involves file IO and/or bitmap work (depending on the frame type)
    ;;   that we can no-op for a free 50-100ms saving in startup time.
    (advice-add #'display-startup-screen :override #'ignore)))


;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:
;;; early-init.el ends here
