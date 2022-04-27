;;; init-markdown.el --- config markdown-mode        -*- lexical-binding: t; -*-

;;; Commentary:

;; Config markdown-mode

;;; Code:

(setq markdown-command "multimarkdown")
(setq-default visual-fill-column-center-text t)

(require 'markdown-mode)

(defun my/markdown-mode-hook ()
  (visual-line-mode 1)
  (smartparens-mode 1)
  (text-scale-increase 1.2))

(add-hook 'markdown-mode-hook 'my/markdown-mode-hook)
(add-hook 'gfm-mode-hook 'my/markdown-mode-hook)
(add-hook 'visual-line-mode-hook #'visual-fill-column-mode)

(provide 'init-markdown)
;;; init-markdown.el ends here
