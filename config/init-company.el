;;; init-company.el --- company config               -*- lexical-binding: t; -*-


(setq company-idle-delay 0.1)
(setq company-global-modes '(not org-mode markdown-mode eshell-mode))

(global-company-mode 1)
(setq completion-ignore-case t)

(with-eval-after-load 'company-mode
  (define-key company-active-map (kbd "RET") 'company-complete-selection)
  (define-key company-active-map [return] 'company-complete-selection))

(provide 'init-company)

