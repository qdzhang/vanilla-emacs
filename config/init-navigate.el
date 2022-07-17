;;; init-navigate.el --- Configure vim-like jumplist  -*- lexical-binding: t; -*-

;;; Commentary:

;; 

;;; Code:

(require 'jumplist)
(global-set-key (kbd "C-<") 'jumplist-previous)
(global-set-key (kbd "C->") 'jumplist-next)
(custom-set-variables
 '(jumplist-hook-commands
   '(dired-jump isearch-forward isearch-backward end-of-buffer beginning-of-buffer find-file
                xref-find-definitions xref-find-references rg-literal rg-dwim rg-dwim-project-dir
                rg-dwim-current-dir next-error-no-select previous-error-no-select
                bookmark-jump tab-bar-switch-to-tab tab-bar-new-tab))
 '(jumplist-ex-mode t))

(provide 'init-navigate)
;;; init-navigate.el ends here
