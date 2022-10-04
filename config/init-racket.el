;;; init-racket.el --- Racket-mode config            -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

(with-eval-after-load 'racket-repl
  (define-key racket-repl-mode-map (kbd "M-]") 'my/close-all-parentheses))

(provide 'init-racket)
;;; init-racket.el
