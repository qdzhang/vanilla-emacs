;;; init-flyspell.el --- config flyspell             -*- lexical-binding: t; -*-

;;; Commentary:

;; flyspell setup

;;; Code:

(with-eval-after-load 'flyspell
  (setq ispell-program-name (executable-find "hunspell")
        ispell-dictionary "en_US"))

(provide 'init-flyspell)
;;; init-flyspell.el ends here
