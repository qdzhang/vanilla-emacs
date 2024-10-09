;;; init-company.el --- company config               -*- lexical-binding: t; -*-

;;; Commentary:

;; Disable `global-company-mode' default, use built-in completion. See the file
;; `init-completion.el' for the details.
;; When demand `company-mode', press <f6>(`my/toggle-company-mode').

;;; Code:

(setq company-idle-delay 0.1)
(setq company-global-modes '(not org-mode markdown-mode eshell-mode))
(setq company-minimum-prefix-length 2)
(setq company-auto-commit nil)

(setq completion-ignore-case t)
(setq company-dabbrev-downcase nil)
(setq company-dabbrev-ignore-case t)
(setq company-dabbrev-code-ignore-case t)
(setq company-etags-ignore-case t)
(setq company-require-match nil)

;; Don't use the predefined configuration. Config Company behavior manually.
(setq company-tng-auto-configure nil)


;;; Copied from doom emacs
;;;###autoload
(defun +company/completing-read ()
  "Complete current company candidates in minibuffer.

Uses ivy, helm, vertico, or ido, if available."
  (interactive)
  (cond ((not company-candidates)
         (user-error "No company candidates available"))
        ((when-let (cand (completing-read "Candidate: " company-candidates))
           (company-finish cand)))))

(require 'company-posframe)
(company-posframe-mode 1)
;; Disable the auto quickhelp frame. Use <f1> to open quickhelp manually
(setq company-posframe-quickhelp-delay nil)

(with-eval-after-load 'company-mode
  ;; User `company-tng-mode', Tab and go!
  (define-key company-active-map (kbd "TAB") 'company-select-next)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map [return] 'nil)

  ;; Other userful keybindings:
  ;; C-w: `company-show-location'
  ;; C-h (or <f1>): `company-show-doc-buffer'

  ;; C-s: `company-search-candidates'
  ;; After search, use `C-o'(`company-search-toggle-filtering') can filter the
  ;; candidates by the search string.
  (define-key company-active-map (kbd "C-M-i") 'company-complete)
  (define-key company-active-map (kbd "C-M-s") '+company/completing-read))

(with-eval-after-load 'company-etags
  '(progn (add-to-list 'company-etags-modes 'web-mode)))
(setq company-etags-everywhere '(php-mode html-mode web-mode nxml-mode))


;;;###autoload
(defun my/toggle-company-mode ()
  "Load `company-mode' config and toggle it"
  (interactive)
  (global-company-mode 'toggle)
  (if global-company-mode
      (message "global-company-mode is on")
    (message "global-comany mode is off")))


(provide 'init-company)

