;;; init-menu.el --- Configure context menu after emacs 28  -*- lexical-binding: t; -*-

;;; Commentary:

;; Configure `context-menu-mode' after emacs28

;;; Code:

(context-menu-mode 1)

(defun highlight-symbol-at-mouse (e)
  "Highlight symbol at mouse click E."
  (interactive "e")
  (save-excursion
    (mouse-set-point e)
    (highlight-symbol-at-point)))

(defun context-menu-highlight-symbol (menu click)
  "Populate MENU with command to highlight symbol at point"
  (save-excursion
    (mouse-set-point click)
    (when (symbol-at-point)
      (define-key-after menu [highlight-search-separator] menu-bar-separator)
      (define-key-after menu [highlight-search-mouse]
        '(menu-item "Highlight Symbol" highlight-symbol-at-mouse
                    :help "Highlight symbol at point"))))
  menu)

(add-hook 'context-menu-functions #'context-menu-highlight-symbol)

(provide 'init-menu)
;;; init-menu.el ends here
