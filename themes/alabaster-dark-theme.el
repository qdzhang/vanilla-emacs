(deftheme alabaster-dark
  "Alabaster Dark - Ported from Sublime Text.
The original repository is https://github.com/tonsky/sublime-scheme-alabaster")

(let ((fg      "#CECECE")
      (bg      "#0E1415")
      (active  "#CD974B")
      (selection "#293334")
      (comment   "#DFDF8E")
      (string    "#95CB82")
      (constant  "#CC8BC9")
      (def       "#71ADE7")
      (punct     "#708B8D")
      (error     "#c33")
      (err-bg    "#2B1D1E")
      (line-hl   "#1a2021"))

  (custom-theme-set-faces
   'alabaster-dark

   ;; Basic UI
   `(default ((t (:foreground ,fg :background ,bg))))
   `(cursor ((t (:background ,active))))
   `(menu ((t (:foreground "black"))))
   `(region ((t (:background ,selection))))
   `(hl-line ((t (:background ,line-hl))))
   `(fringe ((t (:background ,bg))))
   `(mode-line ((t (:foreground ,fg :background "#232b2c" :box (:line-width -1 :style released-button)))))
   `(mode-line-inactive ((t (:foreground ,punct :background ,bg :box (:line-width -1 :style silver-fixed)))))

   ;; Syntax Highlighting (Font Lock)
   `(font-lock-comment-face ((t (:foreground ,comment))))
   `(font-lock-string-face ((t (:foreground ,string))))
   `(font-lock-constant-face ((t (:foreground ,constant))))
   `(font-lock-keyword-face ((t (:foreground ,fg))))
   `(font-lock-function-name-face ((t (:foreground ,def))))
   `(font-lock-variable-name-face ((t (:foreground ,fg))))
   `(font-lock-type-face ((t (:foreground ,def))))
   `(font-lock-builtin-face ((t (:foreground ,def))))
   `(font-lock-warning-face ((t (:foreground ,error :weight bold))))
   `(font-lock-negation-char-face ((t (:foreground ,punct))))

   ;; Punctuation & Operators (Common in modern Emacs packages)
   `(font-lock-punctuation-face ((t (:foreground ,punct))))
   `(font-lock-operator-face ((t (:foreground ,punct))))

   ;; Search & Brackets
   `(show-paren-match ((t (:foreground ,active :background "#d33682"))))
   `(isearch ((t (:foreground "#000000" :background ,active))))

   ;; Diff / Markup
   `(diff-added ((t (:foreground "#80a000"))))
   `(diff-removed ((t (:foreground "#d43d3d"))))
   `(diff-changed ((t (:foreground "#f2990d"))))

   ;; Misc
   `(web-mode-current-element-highlight-face ((t :foreground "white" :background "PaleGreen4")))
   `(ansi-color-blue ((t :foreground "#569cd6" :background "#569cd6")))
   `(ansi-color-bright-blue ((t :foreground "#007acc" :background "#007acc")))
   `(underline ((t (:underline t))))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-directory load-file-name)))

(provide-theme 'alabaster-dark)
