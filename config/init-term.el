;;; init-term.el --- config terminal                 -*- lexical-binding: t; -*-

;;; Commentary:

;; Config `term' and `ansi-term'

;;; Code:

(define-key term-raw-map (kbd "M-x") nil)

;; Copied from Spacemacs
(defun my/ansi-term-handle-close ()
  "Close current term buffer when `exit' from term buffer."
  (when (ignore-errors (get-buffer-process (current-buffer)))
    (set-process-sentinel (get-buffer-process (current-buffer))
                          (lambda (proc change)
                            (when (string-match "\\(finished\\|exited\\)"
                                                change)
                              (kill-buffer (process-buffer proc))
                              (when (> (count-windows) 1)
                                (delete-window)))))))

(add-hook 'term-mode-hook 'my/ansi-term-handle-close)

;;;###autoload
(defun my/quick-cd (dir)
  (interactive "Dcd: ")
  (when (eq major-mode 'term-mode)
    (term-send-raw-string (concat "cd " (shell-quote-argument (expand-file-name dir)) "\n"))
    (term-send-raw-string "\C-l")))

(provide 'init-term)
;;; init-term.el ends here
