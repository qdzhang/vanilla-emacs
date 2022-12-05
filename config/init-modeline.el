;;; init-modeline.el --- Config modeline             -*- lexical-binding: t; -*-

;;; Commentary:

;; Config modeline and diminish some minor modes

;;; Code:

;; Hide all minor mode lighter
(setq mode-line-modes
      (mapcar (lambda (elem)
                (pcase elem
                  (`(:propertize (,_ minor-mode-alist . ,_) . ,_)
                   "")
                  (_ elem)))
              mode-line-modes))

(provide 'init-modeline)
;;; init-modeline.el ends here
