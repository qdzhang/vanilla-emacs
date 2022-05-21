;;; init-modeline.el --- Config modeline             -*- lexical-binding: t; -*-

;;; Commentary:

;; Config modeline and diminish some minor modes

;;; Code:

(custom-theme-set-faces
 'user
 `(mode-line ((t (:inherit variable-pitch :height 1.0 :box (:line-width 2 :color "#000000") :background "#c1c1c1" :foreground "#000000"))))
 `(mode-line-inactive ((t (:inherit variable-pitch :height 1.0 :box (:line-width 2 :color "#676767") :background "#eeeeee" :foreground "#000000")))))

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
