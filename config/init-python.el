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


(provide 'init-python)
;;; init-python.el
