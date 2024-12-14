;;; railscast-theme.el --- railscast theme

;; Copyright (C) 2009 Oleg Shaldybin <oleg.shaldybin@gmail.com>
;; Copyright (C) 2014 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/emacs-jp/replace-colorthemes
;; Version: 0.01

;;; License:

;; MIT License
;; -----------
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use,
;; copy, modify, merge, publish, distribute, sublicense, and/or
;; sell copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following
;; conditions:
;;
;; The above copyright notice and this permission notice shall
;; be included in all copies or substantial portions of the
;; Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:
;;
;; Port of railscast theme from https://github.com/olegshaldybin/color-theme-railscasts.git

;;; Code:

(deftheme railscast
  "railscast theme")

(custom-theme-set-faces
 'railscast

 '(default ((t (:background "#232323" :foreground "#E6E1DC"))))
 '(border ((t (:foreground "#232323"))))
 '(mouse ((t (:foreground "sienna1"))))
 '(cursor ((t (:background "#FFC66D"))))

 '(blue ((t (:foreground "blue"))))
 '(bold ((t (:bold t))))
 '(bold-italic ((t (:italic t :bold t))))
 '(fringe ((t (:background "#232323"))))
 '(font-lock-builtin-face ((t (:foreground "#D0D0FF"))))
 '(font-lock-comment-face ((t (:foreground "#BC9458" :italic t))))
 '(font-lock-constant-face ((t (:foreground "#6D9CBE"))))
 '(font-lock-doc-string-face ((t (:foreground "#A5C261"))))
 '(font-lock-function-name-face ((t (:foreground "#FFC66D"))))
 '(font-lock-keyword-face ((t (:foreground "#CC7833"))))
 '(font-lock-preprocessor-face ((t (:foreground "#CC7833"))))
 '(font-lock-reference-face ((t (:foreground "LightSteelBlue"))))
 '(font-lock-string-face ((t (:foreground "#A5C261"))))
 '(font-lock-type-face ((t (:foreground "white"))))
 '(font-lock-variable-name-face ((t (:foreground "LightSteelBlue"))))
 '(font-lock-warning-face ((t (:foreground "Pink"))))
 '(paren-face-match-light ((t (:foreground "#FFC66D" :background "#555577"))))
 '(highlight ((t (:background "darkolivegreen"))))
 '(italic ((t (:italic t))))
 '(minibuffer-prompt ((t (:foreground "#729fcf")))) ; light sky blue
 ;; '(minibuffer-prompt ((t (:bold t :foreground "#FF6600"))))
 '(region ((t (:background "#555577"))))
 '(primary-selection ((t (:background "#555577"))))
 '(show-paren-match ((t :background "PaleGreen4")))
 '(isearch ((t (:background "#555555"))))
 '(zmacs-region ((t (:background "#555577"))))
 '(secondary-selection ((t (:background "darkslateblue"))))
 '(flymake-errline ((t (:background "LightSalmon" :foreground "black"))))
 '(flymake-warnline ((t (:background "LightSteelBlue" :foreground "black"))))
 '(web-mode-current-element-highlight-face ((t :foreground "white" :background "PaleGreen4")))
 '(mode-line
   ((t ( :background "darkslateblue"
         :foreground "white"
         :box (:line-width -1 :style released-button)))))
 '(mode-line-buffer-id ((t ( :bold t
                             :background "darkslateblue"
                             :foreground "white"
                             :weight bold))))
 '(mode-line-emphasis ((t (:bold t :weight bold))))
 '(mode-line-highlight
   ((t (:box (:line-width 2 :color "grey40" :style released-button)))))
 '(mode-line-inactive
   ((t ( :background "grey30"
         :foreground "grey80"
         :box (:line-width -1 :color "grey40" :style nil)
         :weight light))))
 '(mode-line-mousable ((t (:background "darkslateblue" :foreground "white"))))
 '(mode-line-mousable-minor-mode
   ((t (:background "darkslateblue" :foreground "white"))))
 '(dired-header ((t ( :bold t
                      :weight bold
                      :background "darkslateblue"
                      :foreground "white"))))
 '(ansi-color-blue ((t :foreground "#569cd6" :background "#569cd6")))
 '(ansi-color-bright-blue ((t :foreground "#007acc" :background "#007acc")))
 '(underline ((t (:underline t)))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'railscast)

;;; railscast-theme.el ends here
