;;; init-menu.el --- Configure context menu after emacs 28  -*- lexical-binding: t; -*-

;;; Commentary:

;; Configure `context-menu-mode' after emacs28

;;; Code:

(context-menu-mode 1)

(add-hook 'context-menu-functions #'context-menu-ffap)

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
      (let ((submenu (make-sparse-keymap (propertize "Highlight"))))
        (define-key-after submenu [highlight-search-mouse]
          `(menu-item "Highlight Symbol" highlight-symbol-at-mouse
                      :help "Highlight symbol at point"))
        (define-key-after submenu [unhighlight-symbol]
          `(menu-item "Unhighlight Regexp" unhighlight-regexp
                      :help "Unhighlight selected regexp"))
        (define-key-after submenu [unhighlight-all]
          `(menu-item "Unhighlight All" (lambda (click)
                                          (interactive "e")
                                          (unhighlight-regexp t))))
        (define-key-after menu [highlight-search-mouse]
          `(menu-item "Highlight" ,submenu)))))
  menu)

(add-hook 'context-menu-functions #'context-menu-highlight-symbol)

(defun context-menu-inf-sdcv (menu click)
  "Menu entry to search current word using inf-sdcv"
  (define-key-after menu [inf-sdcv]
    '(menu-item "Inf-sdcv" (lambda (click)
                             (interactive "e")
                             (require 'inf-sdcv-mode)
                             (inf-sdcv-search-at-point))
                :help "Search current word using inf-sdcv"))
  menu)

(add-hook 'context-menu-functions #'context-menu-inf-sdcv)

(provide 'init-menu)
;;; init-menu.el ends here
