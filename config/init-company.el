;;; init-company.el --- company config               -*- lexical-binding: t; -*-


(setq company-idle-delay 0.1)
(setq company-global-modes '(not org-mode markdown-mode eshell-mode))

(global-company-mode 1)
(setq completion-ignore-case t)
(setq company-dabbrev-downcase nil)

(require 'company-posframe)
(company-posframe-mode 1)
;; Disable the auto quickhelp frame. Use <f1> to open quickhelp manually
(setq company-posframe-quickhelp-delay nil)

(with-eval-after-load 'company-mode
  (define-key company-active-map (kbd "RET") 'company-complete-selection)
  (define-key company-active-map [return] 'company-complete-selection))

(provide 'init-company)

