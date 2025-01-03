;;; init-super-save.el --- super-save config         -*- lexical-binding: t; -*-


(setq auto-save-default nil)

(require 'super-save)


(add-to-list 'super-save-triggers 'my/quick-window-jump)
(add-to-list 'super-save-triggers 'magit-status)
(add-to-list 'super-save-hook-triggers 'find-file-hook)
(add-to-list 'super-save-triggers 'switch-to-buffer)
(add-to-list 'super-save-triggers 'tab-bar-switch-to-tab)

(super-save-mode +1)

(setq super-save-auto-save-when-idle t)
(setq super-save-exclude '(".gpg"))
;; Enable deleting trailing white spaces before saving (except for the current line)
(setq super-save-delete-trailing-whitespace 'except-current-line)
;; Save silently
(setq super-save-silent t)
;; Save all buffer
(setq super-save-all-buffers t)
;; Don't auto save remote file
(setq super-save-remote-files nil)

(provide 'init-super-save)
