;;; init-lazy-keys.el --- Init for lazy load keys

;;; newline-without-break-of-line
;;;###autoload
(defun my/newline-without-break-of-line ()
  "1. move to end of the line.
  2. insert newline with index"

  (interactive)
  (let ((oldpos (point)))
    (end-of-line)
    (newline-and-indent)))

(global-set-key (kbd "<C-return>") 'my/newline-without-break-of-line)

;;; === Git config ===

(lazy-load-global-keys
 '(
   ("C-c g" . one-key-menu-magit))
 "init-git")

;;; === Git config ===

(provide 'init-lazy-keys)
