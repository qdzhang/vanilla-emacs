;;; init-smartparens.el --- Init for smartparens  -*- lexical-binding: t; -*-

(require 'smartparens-config)

(setq show-smartparens-global-mode nil)
(setq show-smartparens-mode nil)
(add-hook 'prog-mode-hook #'smartparens-strict-mode)

(one-key-create-menu
 "SMARTPARENS"
 '((("d" . "Down") . sp-down-sexp)
   (("e" . "Up") . sp-up-sexp)
   (("u" . "Backward Up") . sp-backward-up-sexp)
   (("a" . "Backward Down") . sp-backward-down-sexp)
   (("f" . "Forward") . sp-forward-sexp)
   (("b" . "Backward") . sp-backward-sexp)
   (("k" . "Kill") . sp-kill-sexp)
   (("r" . "Raise") . sp-raise-sexp)
   (("t" . "Transpose") . sp-transpose-sexp)
   (("c" . "Convolute") . sp-convolute-sexp)
   (("j" . "Join") . sp-join-sexp)
   (("(" . "Wrap ()") . sp-wrap-round)
   (("{" . "Wrap {}") . sp-wrap-curly)
   (("[" . "Wrap []") . sp-wrap-square)
   )
 t)

(defun my/sp-new-line ()
  "This is my version of `sp-newline'.
Jump out a sexp, then call `sp-newline'"
  (interactive)
  (sp-up-sexp)
  (sp-newline))

(provide 'init-smartparens)
