;;; init-term.el --- config terminal                 -*- lexical-binding: t; -*-

;;; Commentary:

;; Config `term' and `ansi-term'

;;; Code:

;;; A general function to create a new split window and switch to it
;;; https://emacsredux.com/blog/2013/04/29/start-command-or-switch-to-its-buffer/
(defun my/start-or-switch-to-split (function buffer-name)
  "Invoke FUNCTION if there is no buffer with BUFFER-NAME.
Otherwise switch to the buffer named BUFFER-NAME.  Don't clobber
the current buffer."
  (if (not (get-buffer buffer-name))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (funcall function))
    (switch-to-buffer-other-window buffer-name)))

;; Open ansi-term in a split window
(defun my/open-term-in-split-window ()
  "Start `ansi-term' in a new split window."
  (interactive)
  (my/start-or-switch-to-split (lambda ()
                                 (ansi-term (executable-find "bash")))
                               "*ansi-term*"))

(defun my/ansi-term-bash ()
  "Start a ternimal emulator using bash without confirming"
  (interactive)
  (ansi-term "/bin/bash"))

(defun my/ansi-term-cycle (reverse)
  (unless reverse
    (when (derived-mode-p 'term-mode)
      (bury-buffer)))
  (let ((buffers (buffer-list)))
    (when reverse
      (setq buffers (nreverse buffers)))
    (catch 'loop
      (dolist (buf buffers)
        (when (with-current-buffer buf (derived-mode-p 'term-mode))
          (switch-to-buffer buf)
          (throw 'loop nil))))))

(defun my/ansi-term-next ()
  "Cycle through ansi-term buffers."
  (interactive)
  (my/ansi-term-cycle nil))

(defun my/term-buffer-exists-p ()
  "Boolean if term-mode buffers exist."
  (catch 'loop
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (derived-mode-p 'term-mode)
          (throw 'loop t))))))

(defun my/ansi-term-toggle ()
  "Toggle `ansi-term'.

If current buffer is ansi-term, close it; Otherwise open a new ansi-term window"
  (interactive)
  (if (derived-mode-p 'term-mode)
      (progn
        (bury-buffer (get-buffer "*ansi-term*"))
        (bury-buffer))
    (unless (my/term-buffer-exists-p)
      (my/ansi-term-bash))
    (my/ansi-term-next)))

(defun my/ansi-term-split-toggle ()
  "Like `my/ansi-term-toggle', but for split term buffer."
  (interactive)
  (if (derived-mode-p 'term-mode)
      (progn
        (bury-buffer (get-buffer "*ansi-term*"))
        (bury-buffer))
    (unless (my/term-buffer-exists-p)
      (my/open-term-in-split-window))
    (my/ansi-term-next)))

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

(provide 'init-term)
;;; init-term.el ends here
