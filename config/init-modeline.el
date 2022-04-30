;;; init-modeline.el --- Config modeline             -*- lexical-binding: t; -*-

;;; Commentary:

;; Config modeline and diminish some minor modes

;;; Code:

(require 'rich-minority)
(rich-minority-mode 1)

(setq useless-minor-modes
      '(" hl-p"
        "super-save"
        "ElDoc"
        "SP"
        "ws"
        "=>"))
(setq rm-blacklist (mapconcat 'identity useless-minor-modes "\\| "))

(with-eval-after-load 'rich-minority
  (add-to-list 'rm-text-properties '("\\` God\\'" 'face '(:foreground "red"))))

(provide 'init-modeline)
;;; init-modeline.el ends here
