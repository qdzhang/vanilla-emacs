;;; init-eshell.el --- eshell config                 -*- lexical-binding: t; -*-

(defun my/eshell-history ()
  (interactive)
  (require 'em-hist)
  (let* ((start-pos (save-excursion (eshell-bol) (point)))
         (end-pos (point))
         (input (buffer-substring-no-properties start-pos end-pos))
         (command (completing-read "History: "
                                   (when (> (ring-size eshell-history-ring) 0)
                                     (ring-elements eshell-history-ring)))))
    (setf (buffer-substring start-pos end-pos) command)
    (end-of-line)))

(provide 'init-eshell)