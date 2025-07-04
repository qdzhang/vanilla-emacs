;;; mono-dark-theme.el --- Monochrome dark theme           -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(deftheme mono-dark
  "mono-dark theme")

(custom-theme-set-faces
 'mono-dark
 '(default ((t :background "#181818" :foreground "#e4e4ef")))
 '(cursor ((t :background "#e0e0e0")))
 '(font-lock-comment-face ((t :foreground "violet")))
 '(font-lock-keyword-face ((t (:foreground unspecified :background unspecified))))
 '(font-lock-operator-face ((t (:foreground unspecified))))
 '(font-lock-type-face ((t (:foreground unspecified))))
 '(font-lock-variable-name-face ((t (:foreground unspecified :background unspecified))))
 '(font-lock-constant-face ((t (:foreground unspecified :background unspecified))))
 '(font-lock-number-face ((t (:foreground unspecified))))
 '(font-lock-doc-face ((t (:foreground unspecified :inherit 'font-lock-comment-face))))
 '(font-lock-preprocessor-face ((t (:foreground unspecified))))
 '(font-lock-builtin-face ((t (:foreground unspecified))))
 '(ansi-color-white ((t :background "gray90" :foreground "gray90")))
 '(region ((t :background "#005959")))
 '(show-paren-match ((t :background "#d33682")))
 '(header-line ((t :inherit mode-line-inactive)))
 '(mode-line ((t :background "#505050" :foreground "#f6f3e8")))
 '(mode-line-active ((t :inherit mode-line :box (:line-width -1 :style released-button))))
 '(mode-line-inactive ((t :background "#323232" :foreground "#a6a6a6" :box (:line-width -1 :color "#606070"))))
 '(web-mode-current-element-highlight-face ((t :foreground "white" :background "PaleGreen4")))
 '(ansi-color-blue ((t :foreground "#569cd6" :background "#569cd6")))
 '(ansi-color-bright-blue ((t :foreground "#007acc" :background "#007acc")))
 '(underline ((t (:underline t))))
 '(cperl-array-face ((t (:bold t :foreground "DeepSkyBlue"))))
 '(cperl-hash-face ((t (:italic t :bold t :foreground "tomato"))))
 '(cperl-nonoverridable-face ((t (:foreground "chartreuse3")))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'mono-dark)
;;; mono-dark-theme.el ends here
