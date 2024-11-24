;;; init-python.el --- python relative setup         -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

(add-hook 'python-mode-hook
          (lambda ()
            (smart-dash-mode 1)
            (hs-minor-mode 1)
            (setq imenu-create-index-function 'python-imenu-create-flat-index)
            (imenu-add-menubar-index)
            (my/py-indent-style)
            (eglot-ensure)
            (setq-local compile-command
                        (concat "pytest -v "
                                (when buffer-file-name
                                  (shell-quote-argument buffer-file-name)))
                        compilation-scroll-output t)))

(defun my/py-indent-style ()
  (setq python-indent-offset 4
        tab-width 4
        python-indent-guess-indent-offset nil))

(add-hook 'inferior-python-mode-hook
          (lambda ()
            (smart-dash-mode 1)))

;; Highlight debugger lines
(defun my/python-maybe-highlight-debugger-keywords ()
  "Highlight break point lines."
  (interactive)
  (highlight-lines-matching-regexp "breakpoint()"))

(add-hook 'python-mode-local-vars-hook 'my/python-maybe-highlight-debugger-keywords)

(defun my/django-shell ()
  "Launch django shell.
See https://faridrener.com/2015/09/30/shell-plus-emacs.html"
  (interactive)
  (let ((python-shell-interpreter-args
         (concat "-i "
                 default-directory
                 "manage.py shell")))
    (inheritenv
     (call-interactively 'run-python))))


(provide 'init-python)
;;; init-python.el
