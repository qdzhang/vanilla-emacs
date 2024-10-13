;;; init-treesit.el --- Config built-in treesit      -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;; How to use the built-in treesit in Emacs 29?
;;
;; 1. Compile emacs 29 with tree-sitter. If your package manager (such as pamcan
;; in Arch Linux) handles these packages, this step is easy.
;;
;; 2. Download the tree-sitter grammars (dynamic library object files) from
;; https://github.com/casouri/tree-sitter-module/releases
;;
;; 3. Create a new directory called `tree-sitter' in `user-emacs-directory', put
;; the downloaded grammar libs in it.
;;
;; 4. Test with some predicates:
;; - `treesit-available-p': test if tree-sitter support is built-in and available
;; - `treesit-language-available-p': test if the language exists and is loadable
;; - `treesit-ready-p': test whether the language is ready to be used
;;
;; 5. Use the new `*-ts-mode'. See `init-mode.el' for details.

;;; Code:

(require 'treesit)

;; Use the full theming potential of treesit
(setq treesit-font-lock-level 4)

(provide 'init-treesit)
;;; init-treesit.el
