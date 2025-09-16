;;; init-clang-tools.el --- config some useful clang tools     -*- lexical-binding: t; -*-

;;; Commentary:

;; `clang-format' and `clang-rename' is handy to use.
;; Load system built-in `clang-format.el' and `clang-rename.el'
;; when the function is invocated
;;
;; Notice:
;; Since Clang 20.1.0, the `clang-rename' tool has been removed.
;; Ref: https://releases.llvm.org/20.1.0/tools/clang/docs/ReleaseNotes.html#potentially-breaking-changes

;;; Code:

(load "/usr/share/clang/clang-format.el")

(setq-default clang-format-style "file")
(setq-default clang-format-fallback-style "llvm")

(provide 'init-clang-tools)
;;; init-clang-tools.el ends here
