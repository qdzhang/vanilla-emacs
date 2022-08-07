;;; init-info.el --- Config Info-mode                -*- lexical-binding: t; -*-

;;; Commentary:

;; 

;;; Code:

(defun my/info-mode-font-setup ()
  "Customize fonts in `Info-mode'"
  (text-scale-increase 1.2)

  (custom-theme-set-faces
   'user
   `(Info-quoted ((t (:foreground "deep pink"))))))

(add-hook 'Info-mode-hook #'my/info-mode-font-setup)

(provide 'init-info)
;;; init-info.el ends here
