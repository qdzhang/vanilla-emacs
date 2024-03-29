;;; init-lisp.el --- Config common lisp environment  -*- lexical-binding: t; -*-

;;; Commentary:

;; Config Common Lisp and Scheme develop environment

;;; Code:

(setq sly-lisp-implementations
      '((sbcl ("sbcl"))
        (clisp ("clisp"))))
(setq inferior-lisp-program "/usr/bin/sbcl")
(setq sly-contribs '(sly-scratch sly-mrepl sly-fancy sly-quicklisp sly-stickers sly-indentation))

(with-eval-after-load 'hyperspec
  ;; Download hyperspec here:
  ;; https://ftp.lispworks.com/pub/software_tools/reference/HyperSpec-7-0.tar.gz
  (let ((hyperspec-dir (expand-file-name "~/.local/share/doc/HyperSpec/")))
    (setq common-lisp-hyperspec-root (concat "file://" hyperspec-dir)
          common-lisp-hyperspec-symbol-table (concat hyperspec-dir "Data/Map_Sym.txt"))))

(with-eval-after-load 'sly
  (define-key sly-prefix-map (kbd "M-h") 'sly-documentation-lookup)
  (defalias 's 'sly)

  (defun my/sly-eval-print-last-expression (string)
    "Like `sly-eval-print-last-expression', but without newline for better
indentation"
    (interactive (list (sly-last-expression)))
    (sly-eval-print-last-expression string)
    (forward-line 0)
    (delete-indentation)))

(with-eval-after-load 'sly-mrepl
  (define-key sly-mrepl-mode-map (kbd "M-]") 'my/close-all-parentheses))

(with-eval-after-load 'geiser-repl
  (define-key geiser-repl-mode-map (kbd "M-]") 'my/close-all-parentheses))

(provide 'init-lisp)
;;; init-lisp.el ends here
