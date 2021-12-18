;;; init-aggressive-indent.el --- Init for aggressive-indent

(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
(add-hook 'lisp-mode-hook #'aggressive-indent-mode)

(provide 'init-aggressive-indent)
