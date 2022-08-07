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
      shr-width 120                               ; Fold text to 120 columns
      eww-search-prefix "https://wiby.me/?q=")    ; Use another engine for searching

(setq browse-url-browser-function 'browse-url-firefox)

;; minimal rendering by default
(setq-default shr-inhibit-images t)

(add-hook 'eww-mode-hook (lambda () (text-scale-increase 1.3)))

(transient-define-prefix my-transient/eww-menu ()
  "EWW transient menu"
  [
   :description
   (lambda ()
     (propertize "Welcome to EWW!" 'face 'bold-italic))
   ["Open"
    ("g" "EWW" eww)
    ("f" "Open file" eww-open-file)]
   ["List"
    ("s" "Buffers" eww-list-buffers)
    ("h" "Histories" eww-list-histories)
    ("b" "Bookmarks" eww-list-bookmarks)]
   [""
    ("q" "Quit" keyboard-quit)]])

(define-key eww-mode-map "f" 'browse-url)

(provide 'init-eww)
;;; init-eww.el ends here
