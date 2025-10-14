;;; init-autoload.el --- Config autoload functions   -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Use `my/update-all-autoloads' to generate a single file `my-all-loaddefs.el'
;; that contains all functions should be autoloaded.

;;; Code:

;; Emacs 29+
;;;###autoload
(defun my/update-all-autoloads ()
  (interactive)
  (when (fboundp 'loaddefs-generate)
    (loaddefs-generate
     (list
      (concat user-emacs-directory "site-lisp")
      (concat user-emacs-directory "config"))
     (expand-file-name (concat user-emacs-directory
                               "config/"
                               "my-all-loaddefs.el")))))

(message "Already update all autoloads.")


(provide 'init-autoload)
;;; init-autoload.el
