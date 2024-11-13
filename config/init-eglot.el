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
;; (add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio")))
;; (add-to-list 'eglot-server-programs '(python-mode . ("pylsp")))
;; (add-to-list 'eglot-server-programs '(python-mode . ("ruff-lsp")))
(add-to-list 'eglot-server-programs '(d-mode . ("/usr/bin/serve-d")))

;; Better performance for tsserver and vtsls
(fset #'jsonrpc--log-event #'ignore)
;; Config web-related modes to use vtsls
;; https://github.com/yioneko/vtsls
(add-to-list 'eglot-server-programs '(((js-mode :language-id "javascript")
                                       (js-ts-mode :language-id "javascript")
                                       (tsx-ts-mode :language-id "typescriptreact")
                                       (typescript-ts-mode :language-id "typescript")
                                       (typescript-mode :language-id "typescript"))
                                      "vtsls" "--stdio"))


(add-to-list 'eglot-server-programs
             `((cperl-mode perl-mode) "pls"))


;; Download https://github.com/ltex-plus/ltex-ls-plus
(add-to-list 'eglot-server-programs
             '((org-mode markdown-mode rst-mode) . ("~/LanguageTool/ltex-ls-plus/bin/ltex-ls-plus" "--server-type" "TcpSocket" "--port" :autoport)))
;; Alternative servers for grammar check
;; - Use `harper-ls'
;; (add-to-list 'eglot-server-programs
;;              '((org-mode markdown-mode rst-mode) . ("harper-ls" "--stdio")))
;; - Use `vale-ls'
;; (add-to-list 'eglot-server-programs
;;              '((org-mode markdown-mode rst-mode) . ("vale-ls")))


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

(require 'eglot-doc-posframe)

(with-eval-after-load 'eldoc
  ;; Don't resize echo area display. Use `eglot-doc-posframe' to show long doc.
  (setq eldoc-echo-area-use-multiline-p nil)
  (define-key eglot-mode-map (kbd "M-h") 'eglot-doc-posframe-show))

(provide 'init-eglot)
;;; init-eglot.el ends here
