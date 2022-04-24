;;; init-clang-tools.el --- config some useful clang tools     -*- lexical-binding: t; -*-

;;; Commentary:

;; `clang-format' and `clang-rename' is handy to use.
;; Load system built-in `clang-format.el' and `clang-rename.el'
;; when the function is invocated

;;; Code:

(load "/usr/share/clang/clang-format.el")
(load "/usr/share/clang/clang-rename.el")

(setq-default clang-format-style "file")
(setq-default clang-format-fallback-style "llvm")

(provide 'init-clang-tools)
;;; init-clang-tools.el ends here
