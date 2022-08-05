;;; init-eww.el --- Config eww                       -*- lexical-binding: t; -*-

;;; Commentary:

;; 

;;; Code:

(require 'eww)

(defun my/eww-toggle-images ()
  "Toggle whether images are loaded and reload the current page fro cache."
  (interactive)
  (setq-local shr-inhibit-images (not shr-inhibit-images))
  (eww-reload t)
  (message "Images are now %s"
           (if shr-inhibit-images "off" "on")))

(define-key eww-mode-map (kbd "I") #'my/eww-toggle-images)
(define-key eww-link-keymap (kbd "I") #'my/eww-toggle-images)

(setq shr-use-colors nil                          ; No colours
      shr-indentation 2                           ; Left-side margin
      shr-width 100                               ; Fold text to 70 columns
      eww-search-prefix "https://wiby.me/?q=")    ; Use another engine for searching

;; minimal rendering by default
(setq-default shr-inhibit-images t)

(add-hook 'eww-mode-hook (lambda () (text-scale-increase 1.3)))

(provide 'init-eww)
;;; init-eww.el ends here
