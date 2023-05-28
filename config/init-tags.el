;;; init-tags.el --- Config tags systems             -*- lexical-binding: t; -*-

;;; Commentary:

;; Config etags, ctags and citre

;;; Code:

(defun my/c-project-create-etags (dir-name)
  "Create etags file in a c project."
  (interactive "DDirectory: ")
  (eshell-command
   ;; (format "find %s -type f -name \"*.c\" -o -name \"*.h\" -o -name \"*.cpp\" -o -name \"*.hpp\" | etags --include \"~/tags/TAGS\"  -" dir-name)
   (format "find %s -type f -name \"*.c\" -o -name \"*.h\" -o -name \"*.cpp\" -o -name \"*.hpp\" | etags -" dir-name)))

(defun my/ruby-project-create-ctags ()
  "Create ctags file in a ruby project."
  (interactive)
  (shell-command "ctags -R --languages=ruby --exclude=.git --exclude=log --exclude=tmp . $(bundle list --paths)"))

;; Use global TAGS file
;; (setq tags-table-list '("TAGS" "~/tags/TAGS"))

;; Setup citre
;;
;; (require 'citre)
;; (require 'citre-config)

;; Setup for rbtagger
;;
;; Make tag search case sensitive. Highly recommended for
;; maximum precision.
(setq tags-case-fold-search nil)

;; Reread TAGS without querying if it has changed
(setq tags-revert-without-query 1)

;; Always start a new tags list (do not accumulate a list of
;; tags) to keep up with the convention of one TAGS per project.
(setq tags-add-tables nil)

(provide 'init-tags)
;;; init-tags.el ends here
