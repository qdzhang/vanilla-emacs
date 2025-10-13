;;; init-go.el --- Config for Golang                 -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:


(add-hook 'go-ts-mode-hook (lambda ()
                             (subword-mode +1)
                             ;; (eglot-ensure)
                             (gofmt-on-save-mode)
                             (setq-local truncate-lines t)
                             (setq-local indent-tabs-mode t)
                             (setq-local tab-width 4)))

(with-eval-after-load 'eglot
  (setq-default eglot-workspace-configuration
                '((:gopls .
                          ((staticcheck . t)
                           (completeUnimported . t))))))



;; Config `go vet' as a compilation command
;; Copied from golint
;; https://github.com/golang/lint/blob/master/misc/emacs/golint.el
(require 'compile)

(defun govet-buffer-name (mode)
  "*go vet*")

(defun govet-process-setup ()
  "Setup compilation variables and buffer for `govet'."
  (run-hooks 'govet-setup-hook))

(define-compilation-mode govet-mode "govet"
  "Golint is a linter for Go source code."
  (set (make-local-variable 'compilation-scroll-output) nil)
  (set (make-local-variable 'compilation-disable-input) t)
  (set (make-local-variable 'compilation-process-setup-function)
       'govet-process-setup))

;;;###autoload
(defun govet ()
  "Run go vet on the current file and populate the fix list.
Pressing \"C-x `\" jumps directly to the line in your code which
caused the first message."
  (interactive)
  (compilation-start
   (mapconcat #'shell-quote-argument
              (list "go" "vet" (expand-file-name buffer-file-name)) " ")
   'govet-mode
   'govet-buffer-name))

(provide 'init-go)
;;; init-go.el
