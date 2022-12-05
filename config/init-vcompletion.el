;;; init-vcompletion.el --- Config vcomplete mode    -*- lexical-binding: t; -*-

;;; Commentary:

;; Config built-in completions buffer and vcomplete package

;;; Code:

;; * Config built-in completions buffer
;; ** Hide mode-line in completions buffer
(add-hook 'completion-list-mode-hook 'hidden-mode-line-mode)

;; ** Ignore case
(setq completion-ignore-case t)
(setq pcomplete-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)

;; ** Show detailed info in completions buffer
(setq completions-detailed nil)

;; ** Completions buffer styles
(setq completions-format 'one-column)
(setq completion-show-help nil)
(setq completion-show-inline-help nil)
(when (> emacs-major-version 28)
  (setq completion-auto-help 'always))

;; ** fuzzy completing
(add-to-list 'completion-styles 'flex t)

;; * Config `vcomplete-mode'
(require 'vcomplete)
(vcomplete-mode)

;; ** Config position and size of completions buffer
(if (> emacs-major-version 28)
    (setq completions-max-height 30)
  (add-to-list 'display-buffer-alist
               '("\\*Completions\\*"
                 (display-buffer-reuse-window display-buffer-in-side-window)
                 (side . bottom)
                 (window-height . 0.33)
                 (slot . 0))))

(provide 'init-vcompletion)
;;; init-vcompletion.el ends here
