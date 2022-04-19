;;; init-grep.el --- grep config                     -*- lexical-binding: t; -*-

;; A function to use `rgrep' or `vc-git-grep' in a specific directory
;; https://www.manueluberti.eu/emacs/2021/09/10/rgrep-and-vc-git-grep/
(defun mu-recursive-grep (search-term search-path)
  "Recursively search for SEARCH-TERM in SEARCH-PATH."
  (interactive
   (progn
     (unless grep-command
       (grep-compute-defaults))
     (let ((search-term (grep-read-regexp))
           (search-path (expand-file-name
                         (read-directory-name
                          "Directory: " nil default-directory t))))
       (list search-term search-path))))
  (if (vc-root-dir)
      (vc-git-grep search-term "*" search-path)
    (rgrep search-term "*" search-path)))


