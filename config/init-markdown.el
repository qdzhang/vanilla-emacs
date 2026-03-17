;;; init-markdown.el --- config markdown-mode        -*- lexical-binding: t; -*-

;;; Commentary:

;; Config `md-ts-mode'.

;;; Code:

(defun my/markdown-setup ()
  "Setup for markdown files."
  (setq-local visual-fill-column-center-text t)
  (setq-local visual-fill-column-width 70)
  (visual-line-mode 1)
  (visual-fill-column-mode 1))

(add-hook 'md-ts-mode-hook 'my/markdown-setup)

(provide 'init-markdown)
;;; init-markdown.el ends here
