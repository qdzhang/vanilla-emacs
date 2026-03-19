;;; init-julia.el --- Config Julia lang              -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

(require 'julia-mode)

(add-hook 'julia-mode-hook 'julia-repl-mode)
(with-eval-after-load 'julia-repl
  (julia-repl-set-julia-editor 'emacs))

(provide 'init-julia)
;;; init-julia.el
