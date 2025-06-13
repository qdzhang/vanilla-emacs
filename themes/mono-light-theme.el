;;; mono-light-theme.el --- Monochrome light theme         -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(deftheme mono-light
  "mono-light theme")

(custom-theme-set-faces
 'mono-light

 '(default ((t (:background "white" :foreground "black"))))
 '(cursor ((t (:background "grey15"))))
 '(menu ((t (:foreground "black"))))
 '(font-lock-comment-face ((t :foreground "Firebrick")))
 '(font-lock-keyword-face ((t (:foreground unspecified :background unspecified))))
 '(font-lock-operator-face ((t (:foreground unspecified))))
 '(font-lock-type-face ((t (:foreground unspecified))))
 '(font-lock-variable-name-face ((t (:foreground unspecified :background unspecified))))
 '(font-lock-constant-face ((t (:foreground unspecified :background unspecified))))
 '(font-lock-number-face ((t (:foreground unspecified))))
 '(font-lock-doc-face ((t (:foreground unspecified :inherit 'font-lock-comment-face))))
 '(font-lock-preprocessor-face ((t (:foreground unspecified))))
 '(font-lock-builtin-face ((t (:foreground unspecified))))
 ;; Make `term' and `ansi-term' prompt more distinguishable
 '(ansi-color-white ((t :background "gray65" :foreground "gray65")))
 '(region ((t :background "#eedc82")))
 '(show-paren-match ((t :background "turquoise")))
 '(header-line ((t :inherit mode-line-inactive)))
 '(mode-line ((t :background "grey75" :foreground "black")))
 '(mode-line-active ((t :inherit mode-line :box (:line-width -1 :style released-button))))
 '(mode-line-inactive ((t :background "#e5e5e5" :foreground "#7f7f7f" :box (:line-width -1 :color "#bababa"))))
 '(web-mode-current-element-highlight-face ((t :foreground "black" :background "PaleGreen2")))
 '(highlight ((t (:background "darkseagreen2"))))
 '(secondary-selection ((t (:background "NavyBlue" :foreground "white"))))
 '(cperl-array-face ((t (:bold t :foreground "RoyalBlue" :background "lavender"))))
 '(cperl-hash-face ((t (:italic t :bold t :foreground "tomato" :background "MistyRose"))))
 '(cperl-nonoverridable-face ((t (:foreground "#008b8b")))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'mono-light)
;;; mono-light-theme.el ends here
