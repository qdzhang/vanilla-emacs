;;; init-window.el --- Config emacs windows          -*- lexical-binding: t; -*-

;;; Commentary:

;; Config emacs windows facilites

;;; Code:

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

(global-set-key (kbd "C-x C-1") 'delete-other-windows)
(global-set-key (kbd "C-x C-2") 'split-window-below)
(global-set-key (kbd "C-x C-3") 'split-window-right)
(global-set-key (kbd "C-x C-0") 'delete-window)
(global-set-key (kbd "C-x C-5") 'my/toggle-frame-split)

(defun windmove-prefix ()
  (interactive)
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd "<left>") 'windmove-left)
     (define-key map (kbd "<right>") 'windmove-right)
     (define-key map (kbd "<up>") 'windmove-up)
     (define-key map (kbd "<down>") 'windmove-down)
     (define-key map (kbd "^") 'enlarge-window)
     (define-key map (kbd "-") 'shrink-window)
     map)
   t))

(defun other-window-and-beyond (count &optional all-frames)
  (interactive "p")
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd "o") 'other-window) map)
   t)
  (other-window count all-frames))

(define-key (current-global-map) (kbd "C-x o") 'other-window-and-beyond)
(define-key (current-global-map) (kbd "C-x w") 'windmove-prefix)

(provide 'init-window)
;;; init-window.el ends here
