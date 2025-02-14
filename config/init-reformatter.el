;;; init-reformatter.el --- Reformatter config       -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

(require 'reformatter)

;;;###autoload (autoload 'stree-format-buffer "init-reformatter" nil t)
;;;###autoload (autoload 'stree-format-region "init-reformatter" nil t)
;;;###autoload (autoload 'stree-format-on-save-mode "init-reformatter" nil t)
(reformatter-define stree-format
  :program "stree"
  :args '("format" "--print-width=80")
  :lighter " STREE")

;;;###autoload (autoload 'erb-format-buffer "init-reformatter" nil t)
;;;###autoload (autoload 'erb-format-region "init-reformatter" nil t)
;;;###autoload (autoload 'erb-format-on-save-mode "init-reformatter" nil t)
(reformatter-define erb-format
  :program "erb-formatter"
  :args `("--stdin-filename" ,(buffer-file-name) "--print-width" "80")
  :lighter " ERB")

;;;###autoload (autoload 'shfmt-buffer "init-reformatter" nil t)
;;;###autoload (autoload 'shfmt-region "init-reformatter" nil t)
;;;###autoload (autoload 'shfmt-on-save-mode "init-reformatter" nil t)
(reformatter-define shfmt
  :program "shfmt"
  :args '()
  :lighter " ShFmt")

;;;###autoload (autoload 'perltidy-buffer "init-reformatter" nil t)
;;;###autoload (autoload 'perltidy-region "init-reformatter" nil t)
;;;###autoload (autoload 'perltidy-on-save-mode "init-reformatter" nil t)
(reformatter-define perltidy
  :program "perltidy"
  :args '( "--standard-output"
           "--standard-error-output"
           "--force-read-binary"
           "--quiet"

           ;; FORMATTING OPTIONS
           "--no-check-syntax")
  :lighter " Perltidy")

;;;###autoload (autoload 'gofmt-buffer "init-reformatter" nil t)
;;;###autoload (autoload 'gofmt-region "init-reformatter" nil t)
;;;###autoload (autoload 'gofmt-on-save-mode "init-reformatter" nil t)
(reformatter-define gofmt
  :program "gofmt"
  :args '()
  :lighter " gofmt")

;;;###autoload (autoload 'denofmt-buffer "init-reformatter" nil t)
;;;###autoload (autoload 'denofmt-region "init-reformatter" nil t)
;;;###autoload (autoload 'denofmt-on-save-mode "init-reformatter" nil t)
(reformatter-define denofmt
  :program "deno"
  :args `("fmt"
          "--ext"
          "js"
          "-")
  :lighter " Denofmt")


;;;###autoload (autoload 'ruff-format-buffer "init-reformatter" nil t)
;;;###autoload (autoload 'ruff-format-region "init-reformatter" nil t)
;;;###autoload (autoload 'ruff-format-on-save-mode "init-reformatter" nil t)
(reformatter-define ruff-format
  :program "ruff"
  :args (list "format" "--stdin-filename" (or (buffer-file-name) input-file))
  :lighter " RuffFmt"
  :group 'ruff-format)

(provide 'init-reformatter)
;;; init-reformatter.el
