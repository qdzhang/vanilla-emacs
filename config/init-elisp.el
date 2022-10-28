;;; init-elisp.el --- Config elisp                   -*- lexical-binding: t; -*-

;;; Commentary:

;; Configurations for `emacs-lisp-mode'.

;;; Code:

;; * Config emacs-lisp outline marker
;; Inspired from outlined-mode
;; https://github.com/zk-phi/outlined-elisp-mode
(defcustom outlined-elisp-regexp (rx (or (group line-start
                                                ";;"
                                                whitespace
                                                (one-or-more "*")
                                                whitespace)
                                         (group line-start
                                                ";;;"
                                                (zero-or-more ";")
                                                whitespace)))
  "regexp that matches headings"
  :group 'outlined-elisp)

(defun my/outline-regexp-for-emacs-lisp-mode (&rest _)
  "outline-regexp = +emacs-lisp-outline-regexp"
  (setq-local outline-regexp outlined-elisp-regexp))

(add-hook 'emacs-lisp-mode-hook #'my/outline-regexp-for-emacs-lisp-mode -90)

(add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)

(defun my/ielm-mode-hook ()
  (and (featurep 'smartparens) (smartparens-mode 1)))

(add-hook 'ielm-mode-hook 'my/ielm-mode-hook)

(provide 'init-elisp)
;;; init-elisp.el ends here
