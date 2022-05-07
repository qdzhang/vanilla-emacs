;;; init-god.el --- Config god-mode                  -*- lexical-binding: t; -*-

;;; Commentary:

;; Config god-mode

;;; Code:

(setq god-mode-enable-function-key-translation nil)

(require 'god-mode)
(require 'god-mode-isearch)

(add-to-list 'god-exempt-major-modes 'sly-stickers--replay-mode)

(defun god-update-cursor ()
  "Update my cursor."
  (setq cursor-type
        (if god-local-mode
            'box
          'bar)))

(defun god-toggle-on-overwrite ()
  "Toggle god-mode on overwrite-mode."
  (if (bound-and-true-p overwrite-mode)
      (god-local-mode-pause)
    (god-local-mode-resume)))

(defun my/toggle-frame-split ()
  "If the frame is split vertically, split it horizontally or vice versa.
Assumes that the frame is only split into two."
  (interactive)
  (unless (= (length (window-list)) 2) (error "Can only toggle a frame split in two"))
  (let ((split-vertically-p (window-combined-p)))
    (delete-window) ; closes current window
    (if split-vertically-p
        (split-window-horizontally)
      (split-window-vertically)) ; gives us a split with the other window twice
    (switch-to-buffer nil))) ; restore the original window in this part of the frame

;; Keybindings

(global-set-key (kbd "C-x C-1") 'delete-other-windows)
(global-set-key (kbd "C-x C-2") 'split-window-below)
(global-set-key (kbd "C-x C-3") 'split-window-right)
(global-set-key (kbd "C-x C-0") 'delete-window)
(global-set-key (kbd "C-x C-5") 'my/toggle-frame-split)

(define-key god-local-mode-map (kbd ".") 'repeat)
(define-key god-local-mode-map (kbd "i") #'god-local-mode)

;; https://www.reddit.com/r/emacs/comments/3pk8rl/comment/cw7l5n8/?utm_source=share&utm_medium=web2x&context=3
(defun my/god-esc ()
  "If the minibuffer is open, ESC will close it;
Otherwise, invoke `god-mode-all'"
  (interactive)
  (if (and (>= (recursion-depth) 1)
           (active-minibuffer-window))
      (keyboard-escape-quit)
    (god-mode-all)))
(global-set-key (kbd "<escape>") 'my/god-esc)
(define-key god-local-mode-map (kbd "DEL") 'delete-backward-char)

(define-key isearch-mode-map (kbd "<escape>") 'god-mode-isearch-activate)
(define-key god-mode-isearch-map (kbd "<escape>") 'god-mode-isearch-disable)

;; quickjump is a package that provide functions to navigate like avy
(require 'quickjump)
(define-key god-local-mode-map (kbd "m") 'quickjump-back)
(define-key god-local-mode-map (kbd ",") 'quickjump-forward)

;; Hooks

(add-hook 'god-mode-enabled-hook 'god-update-cursor)
(add-hook 'god-mode-disabled-hook 'god-update-cursor)
(add-hook 'overwrite-mode-hook 'god-toggle-on-overwrite)

(god-mode)

(provide 'init-god)
;;; init-god.el ends here
