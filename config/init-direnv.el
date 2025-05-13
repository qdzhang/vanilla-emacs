;;; init-direnv.el --- Integrate with direnv         -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

(envrc-global-mode)

(defalias 'direnv-reload #'envrc-reload)
(defalias 'direnv-allow #'envrc-allow)

(provide 'init-direnv)
;;; init-direnv.el
