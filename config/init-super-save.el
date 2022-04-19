;;; init-super-save.el --- super-save config         -*- lexical-binding: t; -*-


(setq auto-save-default nil)

(require 'super-save)
(super-save-mode +1)

(add-to-list 'super-save-hook-triggers 'find-file-hook)
(setq super-save-exclude '(".gpg"))

(provide 'init-super-save)
