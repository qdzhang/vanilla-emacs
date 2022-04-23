;;; init-clang-format.el --- config clang-format     -*- lexical-binding: t; -*-

;;; Commentary:

;; Load system built-in `clang-format.el' when the function is invocated

;;; Code:

(load "/usr/share/clang/clang-format.el")

(setq-default clang-format-style "llvm")

(provide 'init-clang-format)
;;; init-clang-format.el ends here
