;;; init-mode.el --- Config some autoload modes      -*- lexical-binding: t; -*-


(add-to-list 'auto-mode-alist '("\\.m?jsx?\\'" . js-mode))
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))

;; use cperl-mode instead of perl-mode
(setq auto-mode-alist (rassq-delete-all 'perl-mode auto-mode-alist))
(add-to-list 'auto-mode-alist '("\\.\\([pP]\\([Llm]\\|erl\\|od\\)\\|al\\)\\'" . cperl-mode))

(setq interpreter-mode-alist (rassq-delete-all 'perl-mode interpreter-mode-alist))
(add-to-list 'interpreter-mode-alist '("\\(mini\\)?perl5?" . cperl-mode))

(autoload 'forth-mode "gforth.el")
(autoload 'forth-block-mode "gforth.el")
(add-to-list 'auto-mode-alist '("\\.\\(f\\|fs\\|fth\\|4th\\|frt\\)\\'" . forth-mode))
(add-to-list 'auto-mode-alist '("\\.fb\\'" . forth-block-mode))

;; Mode load
(autoload 'meson-mode "meson-mode")
(autoload 'janet-mode "janet-mode")
(autoload 'markdown-mode "init-markdown")
(autoload 'gfm-mode "init-markdown")
(autoload 'ruby-mode "init-ruby")
(autoload 'inf-ruby-minor-mode "init-ruby")
;; (autoload 'erb-mode "erb-mode")

(provide 'init-mode)
