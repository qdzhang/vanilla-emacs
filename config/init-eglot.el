;;; init-eglot.el --- Config eglot                   -*- lexical-binding: t; -*-

;;; Commentary:

;; Config eglot

;;; Code:

(require 'eglot)

;; Disable eglot events buffer.
(setq eglot-events-buffer-size 0)

;; Close `flymake-mode' when eglot starts.
;; I want to turn on `flymake-mode' manually.
(add-hook 'eglot-managed-mode-hook (lambda () (flymake-mode -1)))

(add-to-list 'eglot-server-programs '((c++-mode c-mode) "ccls"))
;; (add-to-list 'eglot-server-programs '((c++-mode c-mode) . ("clangd" "--clang-tidy")))
(add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio")))
(add-to-list 'eglot-server-programs '(d-mode . ("/usr/bin/serve-d")))


(transient-define-prefix my-transient/eglot
  "Eglot"
  [["Find"
    ("d" "Declaration" eglot-find-declaration)
    ("i" "Implementation" eglot-find-implementation)
    ("D" "Type Definition" eglot-find-typeDefinition)
    ("h" "Doc" eldoc)]
   ["Edit"
    ("r" "Rename" eglot-rename)
    ("a" "Code Actions" eglot-code-actions)
    ("=" "Format Buffer" eglot-format-buffer)
    ("R" "Format Region" eglot-format)]
   ["Manage"
    ("f" "Flymake" flymake-mode)
    ("X" "Shutdown" eglot-shutdown)
    ("C" "Reconnect" eglot-reconnect)
    ("E" "Display Events Buffer" eglot-events-buffer)]])

(transient-define-prefix my-transient/eglot-format
  "Eglot Format"
  [["Format"
    ("=" "Format Buffer" eglot-format-buffer)
    ("R" "Format Region" eglot-format)]])

(transient-define-prefix my-transient/eglot-refactor
  "Eglot Refactor"
  [["Refactor"
    ("r" "Rename" eglot-rename)
    ("a" "Code Actions" eglot-code-actions)]])

(define-key eglot-mode-map (kbd "C-c e") 'my-transient/eglot)

(provide 'init-eglot)
;;; init-eglot.el ends here
