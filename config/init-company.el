;;; init-company.el --- company config               -*- lexical-binding: t; -*-


(setq company-idle-delay 0.1)
(setq company-global-modes '(not org-mode markdown-mode))

(add-hook 'after-init-hook 'global-company-mode)

(with-eval-after-load 'company-mode
  (define-key company-active-map (kbd "RET") 'company-complete-selection)
  (define-key company-active-map [return] 'company-complete-selection))

(provide 'init-company)

