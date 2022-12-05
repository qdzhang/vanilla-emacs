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

(setq my/eww-search-engines
      '(
        ("google" "https://google.com/search?q=%s")
        ("duckduckgo" "https://html.duckduckgo.com/html?q=%s")
        ("wiby" "https://wiby.me/?q=%s")
        ))

(defun my/eww-set-search-engine ()
  "Search for a term using an engine."
  (interactive)
  (let ((engine (car (cdr (assoc (completing-read "Select a search engine:" my/eww-search-engines) my/eww-search-engines)))))
    (setq eww-search-prefix engine)))

(setq shr-use-colors nil                          ; No colours
      shr-indentation 2                           ; Left-side margin
      shr-width 120                               ; Fold text to 120 columns
      )

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
    ("e" "Change search engine" my/eww-set-search-engine)
    ("q" "Quit" keyboard-quit)]])


(require 'eww-lnum)

(with-eval-after-load 'eww
  (define-key eww-mode-map "o" 'browse-url)
  (define-key eww-mode-map "f" 'eww-lnum-follow)
  (define-key eww-mode-map "F" 'eww-lnum-universal)

  (custom-theme-set-faces
   'user
   `(eww-lnum-number ((t (:background "light pink" :foreground "black"))))))

;; Use readability-cli to make eww pages more readable
;; https://gitlab.com/gardenappl/readability-cli
(setq eww-retrieve-command '("readable"))

(provide 'init-eww)
;;; init-eww.el ends here
