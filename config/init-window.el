;;; init-window.el --- Config emacs windows          -*- lexical-binding: t; -*-

;;; Commentary:

;; Config emacs windows facilites

;;; Code:

(require 'windmove)

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


(provide 'init-window)
;;; init-window.el ends here
