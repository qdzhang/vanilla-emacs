;;; init-flymake.el --- Config flymake               -*- lexical-binding: t; -*-

;;; Commentary:

;; Use `flymake-quickdef' to define flymake backends

;;; Code:


(remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)

(require 'flymake-quickdef)

(flymake-quickdef-backend flymake-vale
  :pre-let ((vale-exec (executable-find "vale")))
  :pre-check (unless vale-exec (error "Cannot find vale executable"))
  :write-type 'file
  :proc-form (list vale-exec "--output" "line" fmqd-temp-file)
  :search-regexp "^\\([^:]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\):\\(.*\\):\\(.*\\)$"
  :prep-diagnostic
  (let* ((lnum (string-to-number (match-string 2)))
         (col (string-to-number (match-string 3)))
         (text (match-string 5))
         (pos (flymake-diag-region fmqd-source lnum col))
         (beg (car pos))
         (end (cdr pos))
         (msg (format "%s" text)))
    (list fmqd-source beg end :warning msg)))

(add-hook 'flymake-mode-hook
          (lambda ()
            (require 'flymake-childframe)
            (flymake-childframe-mode 1)))

(provide 'init-flymake)
;;; init-flymake.el ends here
