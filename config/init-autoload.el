;;; init-autoload.el --- Config autoload functions   -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file contains all functions should be autoloaded.

;;; Code:

;;;###autoload
(defun my/update-all-autoloads ()
  (interactive)
  (cd (concat user-emacs-directory "site-lisp"))
  (let ((generated-autoload-file
         (expand-file-name "site-lisp-loaddefs.el")))
    (update-directory-autoloads ""))

  (cd (concat user-emacs-directory "config"))
  (let ((generated-autoload-file
         (expand-file-name "config-loaddefs.el")))
    (update-directory-autoloads "")))


(provide 'init-autoload)
;;; init-autoload.el
