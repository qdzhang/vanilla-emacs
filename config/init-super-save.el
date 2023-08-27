;;; init-super-save.el --- super-save config         -*- lexical-binding: t; -*-


(setq auto-save-default nil)

(require 'super-save)
(super-save-mode +1)

(add-to-list 'super-save-hook-triggers 'find-file-hook)
(add-to-list 'super-save-triggers 'switch-to-buffer)
(add-to-list 'super-save-triggers 'tab-bar-switch-to-tab)
(setq super-save-auto-save-when-idle t)
(setq super-save-exclude '(".gpg"))

(provide 'init-super-save)
