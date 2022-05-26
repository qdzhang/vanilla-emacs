;;; init-mode.el --- Config some autoload modes      -*- lexical-binding: t; -*-


(add-to-list 'auto-mode-alist '("\\.m?jsx?\\'" . js-mode))
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))

;; Mode load
(autoload 'meson-mode "meson-mode")
(autoload 'janet-mode "janet-mode")
(autoload 'markdown-mode "init-markdown")
(autoload 'gfm-mode "init-markdown")
(autoload 'ruby-mode "init-ruby")
(autoload 'inf-ruby-minor-mode "init-ruby")
;; (autoload 'erb-mode "erb-mode")

(provide 'init-mode)
