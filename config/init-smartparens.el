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

(with-eval-after-load 'web-mode
  (sp-local-pair 'web-mode "<" nil :actions :rem))

(with-eval-after-load 'js-mode
  (sp-local-pair 'js-mode "<" nil :actions :rem))

;; Fix the conflict of smartparens and electric-pairs in `cperl-mode'
;; https://github.com/syl20bnr/spacemacs/issues/480#issuecomment-262340062
(with-eval-after-load 'cperl-mode
  (add-hook 'smartparens-enabled-hook  (lambda () (define-key cperl-mode-map "{" nil)))
  (add-hook 'smartparens-disabled-hook  (lambda () (define-key cperl-mode-map "{" 'cperl-electric-lbrace))))

(provide 'init-smartparens)
