;;; init-modeline.el --- Config modeline             -*- lexical-binding: t; -*-

;;; Commentary:

;; Config modeline and diminish some minor modes

;;; Code:

(custom-theme-set-faces
 'user
 `(mode-line ((t (:inherit variable-pitch :height 1.0 :box (:line-width 2 :color "#000000") :background "#c1c1c1" :foreground "#000000"))))
 `(mode-line-inactive ((t (:inherit variable-pitch :height 1.0 :box (:line-width 2 :color "#676767") :background "#eeeeee" :foreground "#000000")))))

(provide 'init-modeline)
;;; init-modeline.el ends here
