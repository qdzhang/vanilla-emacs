;;; init-web.el --- Web reletive configurations      -*- lexical-binding: t; -*-

;;; Commentary:

;; Config html-mode and web-mode

;;; Code:

;; * `mhtml-mode' setup
(with-eval-after-load 'mhtml-mode
  (define-key mhtml-mode-map (kbd "C-c C-c b") 'my-skel/hugo-go-template)
  (define-key mhtml-mode-map (kbd "C-c C-c p") 'my-skel/hugo-go-template-without-whitespaces))

(add-hook 'mhtml-mode-hook 'hs-minor-mode)
;; Rename paired HTML/XML tag
;; In `web-mode', use `web-mode-element-rename' with keybinding `C-c C-e r'
(add-hook 'mhtml-mode-hook 'sgml-electric-tag-pair-mode)

;; * `web-mode' setup
(with-eval-after-load 'web-mode
  ;; Let smartparens handle these
  (setq web-mode-enable-auto-quoting nil)
  (setq web-mode-enable-auto-pairing nil)

  ;; Indent setup
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-attr-indent-offset 2)
  (setq web-mode-comment-style 2)

  ;; Highlight current matched tags, and set its color
  (setq web-mode-enable-current-element-highlight t)
  (set-face-attribute 'web-mode-current-element-highlight-face nil :foreground "black" :background "PaleGreen2"))


;; * `css-mode' setup
(with-eval-after-load 'css-mode
  (setq css-indent-offset 2))


(provide 'init-web)
;;; init-web.el ends here
