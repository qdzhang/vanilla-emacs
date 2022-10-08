;;; init-racket.el --- Racket-mode config            -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

(with-eval-after-load 'racket-repl
  (define-key racket-repl-mode-map (kbd "M-]") 'my/close-all-parentheses))

(add-hook 'racket-mode-hook #'racket-xp-mode)

(with-eval-after-load 'smartparens
  (when (fboundp 'sp-local-pair)
    (sp-local-pair 'racket-mode "'" nil :actions nil)
    (sp-local-pair 'racket-mode "`" nil :actions nil)))

(transient-define-prefix my-transient/racket-mode-menu ()
  "Show menu in `racket-mode'"
  [["mode"
    ("'" "racket repl" racket-repl)
    ("u" "unicode" racket-unicode-input-method-enable)]
   ["edit"
    ("a" "align pairs" racket-align)
    ("A" "unallign pairs" racket-unalign)
    ("r" "run program" racket-run)
    ("R" "rename" racket-xp-rename)]
   ["help"
    ("h" "describe" racket-describe-search)]])

(provide 'init-racket)
;;; init-racket.el
