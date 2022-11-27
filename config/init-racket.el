;;; init-racket.el --- Racket-mode config            -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

(require 'racket-mode)

(with-eval-after-load 'racket-repl
  (define-key racket-repl-mode-map (kbd "M-]") 'my/close-all-parentheses))

(add-hook 'racket-mode-hook
          (lambda ()
            (racket-xp-mode 1)
            (setq racket-show-functions 'racket-show-echo-area)
            (setq-local imenu-create-index-function #'racket-imenu-create-index-function)))

(with-eval-after-load 'smartparens
  (when (fboundp 'sp-local-pair)
    (sp-local-pair 'racket-mode "'" nil :actions nil)
    (sp-local-pair 'racket-mode "`" nil :actions nil)))

(transient-define-prefix my-transient/racket-mode-menu ()
  "Show menu in `racket-mode'"
  [["repl"
    ("'" "racket repl" racket-repl)
    ("d" "send defination" racket-send-definition)
    ("e" "send sexp" racket-send-last-sexp)
    ("sr" "send region" racket-send-region)]
   ["edit"
    ("a" "align pairs" racket-align)
    ("A" "unallign pairs" racket-unalign)
    ("p" "cycle paren" racket-cycle-paren-shapes)
    ("r" "run program" racket-run)
    ("R" "rename" racket-xp-rename)
    ("m" "macros" my-transient/racket-macro-menu)]
   ["modes"
    ("f" "fold tests" racket-fold-all-tests)
    ("F" "unfold tests" racket-unfold-all-tests)
    ("t" "test" racket-test)
    ("i" "unicode" racket-unicode-input-method-enable)
    ("l" "logger" racket-logger)
    ("o" "profile" racket-profile)]
   ["help"
    ("h" "describe" racket-describe-search)]])

(transient-define-prefix my-transient/racket-macro-menu ()
  "Transient menu contains racket macro commands"
  ["Macros"
   ("d" "expand definition" racket-expand-definition)
   ("e" "expand last sexp" racket-expand-last-sexp)
   ("r" "expand region" racket-expand-region)])

(defcustom outlined-racket-regexp (rx (or (group line-start
                                                 ";;"
                                                 whitespace
                                                 (one-or-more "*")
                                                 whitespace)
                                          (group line-start
                                                 ";;;"
                                                 (zero-or-more ";")
                                                 whitespace)))
  "regexp that matches headings"
  :group 'outlined-racket)

(defun my/outline-regexp-for-racket-mode (&rest _)
  "outline-regexp = +racket-outline-regexp"
  (setq-local outline-regexp outlined-racket-regexp))

(add-hook 'racket-mode-hook #'my/outline-regexp-for-racket-mode -90)

(add-hook 'racket-mode-hook 'outline-minor-mode)
(add-hook 'racket-mode-hook 'hs-minor-mode)

(provide 'init-racket)
;;; init-racket.el
