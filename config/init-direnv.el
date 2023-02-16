;;; init-direnv.el --- Integrate with direnv         -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

(add-hook 'ruby-mode-hook 'envrc-mode)
(add-hook 'python-mode-hook 'envrc-mode)

(provide 'init-direnv)
;;; init-direnv.el
