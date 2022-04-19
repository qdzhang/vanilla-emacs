;;; init-c.el --- cc-mode and semantic config        -*- lexical-binding: t; -*-


(defun my/c-semantic-hooks ()
  (semantic-mode 1)
  (local-set-key "\C-c\C-j" 'semantic-ia-fast-jump)
  (local-set-key "\C-c\C-s" 'semantic-ia-show-summary))


(add-hook 'c-mode-hook 'my/c-semantic-hooks)
(add-hook 'c++-mode-hook ' my/c-semantic-hooks)

(with-eval-after-load 'company
  ;; Use `company-clang' to auto complate
  (setq company-backends (delete 'company-semantic company-backends))
  ;; Add `company-c-headers' backends
  (add-to-list 'company-backends 'company-c-headers))

(with-eval-after-load 'semantic
  (advice-add 'semantic-idle-scheduler-function :around #'ignore)

  (global-semanticdb-minor-mode 1)
  (global-semantic-idle-scheduler-mode 1)
  (semantic-add-system-include "/usr/include/gtk-3.0/" 'c-mode))

(require 'meson-mode)
(with-eval-after-load 'company
  (add-hook 'meson-mode-hook 'company-mode))

(provide 'init-c)
