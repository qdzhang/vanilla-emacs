;;; init-smartparens.el --- Init for smartparens  -*- lexical-binding: t; -*-

(require 'smartparens-config)

(setq show-smartparens-global-mode nil)
(setq show-smartparens-mode nil)
(add-hook 'prog-mode-hook #'smartparens-strict-mode)

(one-key-create-menu
 "SMARTPARENS"
 '(
   (("d" . "sp down") . sp-down-sexp)
   (("e" . "sp up") . sp-up-sexp)
   (("u" . "sp backward up") . sp-backward-up-sexp)
   (("a" . "sp backward down") . sp-backward-down-sexp)
   (("f" . "sp forward") . sp-forward-sexp)
   (("b" . "sp backward") . sp-backward-sexp)
   (("k" . "sp kill") . sp-kill-sexp)
   )
 t)

(defun my/sp-new-line ()
  "This is my version of `sp-newline'.
Move a char forward, then call `sp-newline'"
  (interactive)
  (forward-char)
  (when (equal (char-before) 41) ; )
    (sp-newline)))

(provide 'init-smartparens)
