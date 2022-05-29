;;; init-eglot.el --- Config eglot                   -*- lexical-binding: t; -*-

;;; Commentary:

;; Config eglot

;;; Code:

(require 'eglot)

;; Disable eglot events buffer.
(setq eglot-events-buffer-size 0)

(add-to-list 'eglot-server-programs '((c++-mode c-mode) "ccls"))
(add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio")))
(add-to-list 'eglot-server-programs '(d-mode . ("/usr/bin/serve-d")))

(provide 'init-eglot)
;;; init-eglot.el ends here
