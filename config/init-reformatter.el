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

(provide 'init-reformatter)
;;; init-reformatter.el
