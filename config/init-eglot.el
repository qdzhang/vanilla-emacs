;;; init-eglot.el --- Config eglot                   -*- lexical-binding: t; -*-

;;; Commentary:

;; Config eglot

;;; Code:

(require 'eglot)

;; Disable eglot events buffer.
(setq eglot-events-buffer-size 0)

;; Close `flymake-mode' when eglot starts.
;; I want to turn on `flymake-mode' manually.
(setq-local eglot-stay-out-of '(flymake))
(add-hook 'eglot-managed-mode-hook (lambda () (flymake-mode -1)))

(with-eval-after-load 'eldoc
  (setq eldoc-echo-area-use-multiline-p nil))

(add-to-list 'eglot-server-programs '((c++-mode c-mode) "ccls"))
;; (add-to-list 'eglot-server-programs '((c++-mode c-mode) . ("clangd" "--clang-tidy")))
;; (add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio")))
;; (add-to-list 'eglot-server-programs '(python-mode . ("pylsp")))
(add-to-list 'eglot-server-programs '(d-mode . ("/usr/bin/serve-d")))

;;;###autoload
(defun my/eglot-setup-deno ()
  "Setup deno lsp for eglot when editing typescript."
  (interactive)
  (add-to-list 'eglot-server-programs '((js-mode typescript-mode) . (eglot-deno "deno" "lsp")))

  (defclass eglot-deno (eglot-lsp-server) ()
    :documentation "A custom class for deno lsp.")

  (cl-defmethod eglot-initialization-options ((server eglot-deno))
    "Passes through required deno initialization options"
    (list :enable t
          :lint t)))

;;;###autoload
(defun my/eglot-unset-deno ()
  "Unset deno lsp eglot setup. Use tsserver when editing typescript."
  (interactive)
  (setq eglot-server-programs
        (seq-remove (lambda (elt) (equal (cdr elt) '(eglot-deno "deno" "lsp")))
                    eglot-server-programs)))


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
