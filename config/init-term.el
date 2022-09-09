;;; init-term.el --- config terminal                 -*- lexical-binding: t; -*-

;;; Commentary:

;; Config `term' and `ansi-term'

;;; Code:

(eval-after-load "term"
  '(define-key term-raw-map (kbd "C-c C-y") 'term-paste))

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

(defun my/exit-term-kill-buffer ()
  "When `term' or `ansi-term' exit, kill the buffer. If `term' is in a split
window, delete this window also.

URL: https://oremacs.com/2015/01/01/three-ansi-term-tips/"
  (let* ((buff (current-buffer))
         (proc (get-buffer-process buff)))
    (set-process-sentinel
     proc
     `(lambda (process event)
        (if (and (string= event "finished\n")
                 (one-window-p))
            (kill-buffer ,buff)
          (progn (kill-buffer ,buff)
                 (delete-window)))))))

(add-hook 'term-exec-hook 'my/exit-term-kill-buffer)

(provide 'init-term)
;;; init-term.el ends here
