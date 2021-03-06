;;; init-lisp.el --- Config common lisp environment  -*- lexical-binding: t; -*-

;;; Commentary:

;; Config common lisp develop environment

;;; Code:

(setq sly-lisp-implementations
      '((sbcl ("sbcl"))
        (clisp ("clisp"))))
(setq inferior-lisp-program "/usr/bin/sbcl")

(with-eval-after-load 'sly
  (define-key sly-prefix-map (kbd "M-h") 'sly-documentation-lookup)
  (defalias 's 'sly))

(provide 'init-lisp)
;;; init-lisp.el ends here
