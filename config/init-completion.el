;;; init-completion.el --- Config built-in completion  -*- lexical-binding: t; -*-

;;; Commentary:

;; Config Emacs built-in completion

;;; Code:



(autoload 'ffap-file-at-point "ffap")

(defun my/complete-path-at-point+ ()
  "Return completion data for UNIX path at point."
  (let ((fn (ffap-file-at-point))
        (fap (thing-at-point 'filename)))
    (when (and (or fn (equal "/" fap))
               (save-excursion
                 (search-backward fap (line-beginning-position) t)))
      (list (match-beginning 0)
            (match-end 0)
            #'completion-file-name-table :exclusive 'no))))

(add-hook 'completion-at-point-functions
          #'my/complete-path-at-point+
          'append)



(provide 'init-completion)
;;; init-completion.el ends here
