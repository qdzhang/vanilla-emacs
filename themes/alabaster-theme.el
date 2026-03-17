(deftheme alabaster
  "A light theme ported from the Alabaster VS Code theme.
The original repository is https://github.com/tonsky/vscode-theme-alabaster")

(let ((class '((class color) (min-colors 89)))
      (bg      "#F7F7F7")
      (fg      "#000000")
      (gray    "#777777")
      (comment "#AA3731")
      (string  "#448C27")
      (number  "#7A3E9D")
      (entity  "#325CC0")
      (lnum    "#9DA39A")
      (hl      "#F0F0F0")
      (select  "#BFDBFE")
      (cursor  "#007ACC")
      (status  "#DDDDDD")
      (status-fg "#474747"))

  (custom-theme-set-faces
   'alabaster

   ;; Basic UI
   `(default ((,class (:background ,bg :foreground ,fg))))
   `(cursor ((,class (:background ,cursor))))
   `(menu ((t (:foreground "black"))))
   `(region ((,class (:background ,select))))
   `(hl-line ((,class (:background ,hl))))
   `(line-number ((,class (:foreground ,lnum :background ,hl))))
   `(line-number-current-line ((,class (:foreground ,fg :background ,hl))))
   `(fringe ((,class (:background ,bg))))
   `(vertical-border ((,class (:foreground ,status))))

   ;; Syntax Highlighting (Font Lock)
   `(font-lock-comment-face ((,class (:foreground ,comment :slant italic))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,comment))))
   `(font-lock-string-face ((,class (:foreground ,string))))
   `(font-lock-doc-face ((,class (:foreground ,string))))
   `(font-lock-constant-face ((,class (:foreground ,number))))
   `(font-lock-keyword-face ((,class (:foreground ,fg :weight bold))))
   `(font-lock-type-face ((,class (:foreground ,fg))))
   `(font-lock-function-name-face ((,class (:foreground ,entity))))
   `(font-lock-variable-name-face ((,class (:foreground ,fg))))
   `(font-lock-builtin-face ((,class (:foreground ,fg))))
   `(font-lock-warning-face ((,class (:foreground "#660000" :background "#96000014"))))
   `(font-lock-negation-char-face ((,class (:foreground ,number))))
   `(font-lock-preprocessor-face ((,class (:foreground ,entity))))

   ;; Mode Line
   `(mode-line ((,class (:background ,status :foreground ,status-fg :box (:line-width -1 :style released-button)))))
   `(mode-line-inactive ((,class (:background ,hl :foreground ,gray :box (:line-width -1 :style flat-button)))))

   ;; Search
   `(isearch ((,class (:background "#FFBC5D" :foreground ,fg))))

   ;; Diff
   `(diff-added ((,class (:background "#DDFFDD" :foreground "#434343"))))
   `(diff-removed ((,class (:background "#FFDDDD" :foreground "#434343"))))
   `(diff-header ((,class (:background "#DDDDFF" :foreground "#434343"))))

   ;; Org Mode
   `(org-level-1 ((,class (:foreground ,entity :weight bold :height 1.1))))
   `(org-level-2 ((,class (:foreground ,number :weight bold))))
   `(org-level-3 ((,class (:foreground ,string :weight bold))))
   `(org-link ((,class (:foreground ,cursor :underline t))))

   ;; Misc
   `(ansi-color-white ((t :background "gray65" :foreground "gray65")))
   `(web-mode-current-element-highlight-face ((t :foreground "black" :background "PaleGreen2")))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'alabaster)
