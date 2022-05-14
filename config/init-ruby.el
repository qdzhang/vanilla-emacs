;;; init-ruby.el --- Config ruby mode                -*- lexical-binding: t; -*-

;;; Commentary:

;; Config ruby mode

;;; Code:

(require 'inf-ruby)

(autoload 'inf-ruby-minor-mode "inf-ruby" "Run an inferior Ruby process" t)
(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)

(setq ruby-deep-indent-paren nil)


(require 'chruby)
(add-hook 'ruby-mode-hook #'chruby-use-corresponding)

(provide 'init-ruby)
;;; init-ruby.el ends here
