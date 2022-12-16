;;; init-markdown.el --- config markdown-mode        -*- lexical-binding: t; -*-

;;; Commentary:

;; Config markdown-mode

;;; Code:

(setq-default visual-fill-column-center-text t)
(setq visual-fill-column-width 50)

(require 'markdown-mode)

(defun my/markdown-mode-hook ()
  (visual-line-mode 1)
  (smartparens-mode 1)
  (variable-pitch-mode 1))

(add-hook 'markdown-mode-hook 'my/markdown-mode-hook)
(add-hook 'gfm-mode-hook 'my/markdown-mode-hook)
(add-hook 'visual-line-mode-hook #'visual-fill-column-mode)

(add-hook 'markdown-mode-hook
          (lambda ()
            (require 'init-flymake)
            (add-hook 'flymake-diagnostic-functions 'flymake-vale nil t)))

(with-eval-after-load 'markdown-mode
  ;; Patch the default `markdown-edit-code-block' in markdown-mode.
  ;; Make this function to edit yaml frontmatter in markdown file.
  (defun my/markdown-edit-code-block ()
    "My version of `markdown-edit-code-block', make it work on yaml-mode.

Edit Markdown code block in an indirect buffer."
    (interactive)
    (save-excursion
      (if (fboundp 'edit-indirect-region)
          (let* ((bounds (markdown-get-enclosing-fenced-block-construct))
                 (begin (and bounds (not (null (nth 0 bounds))) (goto-char (nth 0 bounds)) (point-at-bol 2)))
                 (end (and bounds(not (null (nth 1 bounds)))  (goto-char (nth 1 bounds)) (point-at-bol 1))))
            (if (and begin end)
                (let* ((indentation (and (goto-char (nth 0 bounds)) (current-indentation)))
                       (lang (markdown-code-block-lang))
                       (mode (or (and lang (markdown-get-lang-mode lang))
                                 ;; Check whether `yaml-mode' is in emacs
                                 (when (and (or (featurep 'yaml-mode)
                                                (fboundp 'yaml-mode))
                                            (equal (cdr (markdown-max-of-seq
                                                         #'car
                                                         (cl-remove-if
                                                          #'null
                                                          (cl-mapcar
                                                           #'markdown-find-previous-prop
                                                           (markdown-get-fenced-block-begin-properties)))))
                                                   'markdown-yaml-metadata-begin))
                                   'yaml-mode)
                                 markdown-edit-code-block-default-mode))
                       (edit-indirect-guess-mode-function
                        (lambda (_parent-buffer _beg _end)
                          (funcall mode)))
                       (indirect-buf (edit-indirect-region begin end 'display-buffer)))
                  ;; reset `sh-shell' when indirect buffer
                  (when (and (not (member system-type '(ms-dos windows-nt)))
                             (member mode '(shell-script-mode sh-mode))
                             (member lang (append
                                           (mapcar (lambda (e) (symbol-name (car e)))
                                                   sh-ancestor-alist)
                                           '("csh" "rc" "sh"))))
                    (with-current-buffer indirect-buf
                      (sh-set-shell lang)))
                  (when (> indentation 0) ;; un-indent in edit-indirect buffer
                    (with-current-buffer indirect-buf
                      (indent-rigidly (point-min) (point-max) (- indentation)))))
              (user-error "Not inside a GFM or tilde fenced code block")))
        (when (y-or-n-p "Package edit-indirect needed to edit code blocks. Install it now? ")
          (progn (package-refresh-contents)
                 (package-install 'edit-indirect)
                 (markdown-edit-code-block))))))

  (define-key markdown-mode-map (kbd "C-c '") 'my/markdown-edit-code-block))




(provide 'init-markdown)
;;; init-markdown.el ends here
