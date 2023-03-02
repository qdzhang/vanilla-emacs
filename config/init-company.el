;;; init-company.el --- company config               -*- lexical-binding: t; -*-

;;; Commentary:

;; Disable `global-company-mode' default, use built-in completion. See the file
;; `init-completion.el' for the details.
;; When demand `company-mode', press <f6>(`my/toggle-company-mode').

;;; Code:

(setq company-idle-delay 0.1)
(setq company-global-modes '(not org-mode markdown-mode eshell-mode))
(setq company-minimum-prefix-length 2)

(setq completion-ignore-case t)
(setq company-dabbrev-downcase nil)
(setq company-dabbrev-ignore-case t)
(setq company-dabbrev-code-ignore-case t)
(setq company-etags-ignore-case t)

(require 'company-posframe)
(company-posframe-mode 1)
;; Disable the auto quickhelp frame. Use <f1> to open quickhelp manually
(setq company-posframe-quickhelp-delay nil)

(with-eval-after-load 'company-mode
  (define-key company-active-map (kbd "RET") 'company-complete-selection)
  (define-key company-active-map [return] 'company-complete-selection))

;;;###autoload
(defun my/toggle-company-mode ()
  "Load `company-mode' config and toggle it"
  (interactive)
  (global-company-mode 'toggle)
  (if global-company-mode
      (message "global-company-mode is on")
    (message "global-comany mode is off")))


(provide 'init-company)

