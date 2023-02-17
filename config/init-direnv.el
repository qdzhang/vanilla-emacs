;;; init-direnv.el --- Integrate with direnv         -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

(dolist (mode '(ruby-mode-hook
                python-mode-hook
                go-mode-hook))
  (add-hook mode 'envrc-mode))

(provide 'init-direnv)
;;; init-direnv.el
