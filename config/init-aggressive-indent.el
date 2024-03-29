;;; init-aggressive-indent.el --- Init for aggressive-indent -*- lexical-binding: t; -*-

(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
(add-hook 'lisp-mode-hook #'aggressive-indent-mode)
(add-hook 'scheme-mode-hook #'aggressive-indent-mode)
(add-hook 'arc-mode-hook #'aggressive-indent-mode)
(add-hook 'racket-mode-hook #'aggressive-indent-mode)

(provide 'init-aggressive-indent)
