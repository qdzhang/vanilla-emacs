;;; init-ruby.el --- Config ruby mode                -*- lexical-binding: t; -*-

;;; Commentary:

;; Config ruby mode

;;; Code:

(require 'inf-ruby)

(autoload 'inf-ruby-minor-mode "inf-ruby" "Run an inferior Ruby process" t)
(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)

(setq ruby-deep-indent-paren nil)

(eval-after-load "hideshow"
  '(add-to-list 'hs-special-modes-alist
                `(ruby-mode
                  ,(rx (or "def" "class" "module" "do" "{" "[" "if" "else" "unless"))
                  ,(rx (or "}" "]" "end"))
                  ,(rx (or "#" "=begin"))
                  ruby-forward-sexp nil)))

(add-hook 'ruby-mode-hook #'hs-minor-mode)

;; Use direnv to instead of chruby
;; (require 'chruby)
;; (add-hook 'ruby-mode-hook #'chruby-use-corresponding)

(with-eval-after-load 'web-mode
  (setq web-mode-markup-indent-offset 2))

(provide 'init-ruby)
;;; init-ruby.el ends here
