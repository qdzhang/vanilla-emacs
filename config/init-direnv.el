;;; init-direnv.el --- Integrate with direnv         -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

(defun my/maybe-enable-envrc-global-mode ()
  "Enable `envrc-global-mode' if `direnv' is installed."
  (when (executable-find "direnv")
    (envrc-global-mode)))

(add-hook 'after-init-hook 'my/maybe-enable-envrc-global-mode)

(provide 'init-direnv)
;;; init-direnv.el
