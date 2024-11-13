;;; init-flymake.el --- Config flymake               -*- lexical-binding: t; -*-

;;; Commentary:

;; Use `flymake-quickdef' to define flymake backends

;;; Code:


(remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)

(require 'flymake)
(require 'flymake-quickdef)

(flymake-quickdef-backend flymake-gcc
  :pre-let ((gcc-exec (executable-find "gcc")))
  :pre-check (unless gcc-exec (error "Cannot find gcc executable"))
  :write-type 'file
  :proc-form (list gcc-exec "-fanalyzer" fmqd-temp-file)
  :search-regexp "^\\(.*.c.*\\|.*.h.*\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\):\\(.*\\):\\(.*\\)$"
  :prep-diagnostic
  (let* ((lnum (string-to-number (match-string 2)))
         (col (string-to-number (match-string 3)))
         (text (match-string 5))
         (pos (flymake-diag-region fmqd-source lnum col))
         (beg (car pos))
         (end (cdr pos))
         (msg (format "%s" text)))
    (list fmqd-source beg end :error msg)))

(defun my/flymake-toggle-diagnostics-buffer ()
  "Toggle the diagnostics buffer when entering/exiting `flymake-mode'.
Ref: https://github.com/progfolio/.emacs.d"
  (let* ((root (vc-root-dir))
         (command (if root
                      #'flymake-show-project-diagnostics
                    #'flymake-show-buffer-diagnostics))
         (window (get-buffer-window
                  (if root
                      (flymake--project-diagnostics-buffer root)
                    (flymake--diagnostics-buffer-name)))))
    (if flymake-mode
        (funcall command)
      (when (window-live-p window)
        (with-selected-window window
          (kill-buffer-and-window))))))

;; (add-hook 'flymake-mode-hook #'my/flymake-toggle-diagnostics-buffer)

(provide 'init-flymake)
;;; init-flymake.el ends here
