;;; init-completion.el --- Config built-in completion  -*- lexical-binding: t; -*-

;;; Commentary:

;; Config Emacs built-in completion.

;;; Code:

;; Add file path completions
;; https://with-emacs.com/posts/tutorials/customize-completion-at-point/
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

(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)

;; Use `completing-read' as completing framework
;; https://old.reddit.com/r/emacs/comments/ulsrb5/what_have_you_recently_removed_from_your_emacs/i822lev/
;;
;; And also see `consult-completion-in-region' at consult pages
;; https://github.com/minad/consult#miscellaneous
(defun completing-read-at-point (start end col &optional pred)
  "Inspired by https://github.com/katspaugh/ido-at-point"
  (if (minibufferp) (completion--in-region start end col pred)
    (let* ((init (buffer-substring-no-properties start end))
           (all (completion-all-completions init col pred (length init)))
           (completion (cond
                        ((atom all) nil)
                        ((and (consp all) (atom (cdr all))) (car all))
                        (t (completing-read "Completions: " col pred t init)))))
      (if completion
          (progn
            (delete-region start end)
            (insert completion)
            t)
        (message "No completions") nil))))

(setq completion-category-defaults nil)
(setq completion-in-region-function
      (lambda (&rest args)
        (apply (if icomplete-mode
                   #'completing-read-at-point
                 #'completion--in-region)
               args)))

(provide 'init-completion)
;;; init-completion.el ends here
