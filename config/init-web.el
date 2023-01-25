;;; init-web.el --- Web relative configurations      -*- lexical-binding: t; -*-

;;; Commentary:

;; Config html-mode and web-mode

;;; Code:

;; * `mhtml-mode' setup
(with-eval-after-load 'mhtml-mode
  ;; Define keybindings to insert hugo template easily
  (define-key mhtml-mode-map (kbd "C-c C-c b") 'my-skel/hugo-go-template)
  (define-key mhtml-mode-map (kbd "C-c C-c p") 'my-skel/hugo-go-template-without-whitespaces))

(add-hook 'mhtml-mode-hook 'hs-minor-mode)
;; Rename paired HTML/XML tag
;; In `web-mode', use `web-mode-element-rename' with keybinding `C-c C-e r'
(add-hook 'mhtml-mode-hook 'sgml-electric-tag-pair-mode)

;; * `web-mode' setup
(with-eval-after-load 'web-mode
  ;; Define keybindings to insert hugo template easily
  (define-key web-mode-map (kbd "C-c C-e g") 'my-skel/hugo-go-template)
  (define-key web-mode-map (kbd "C-c C-e G") 'my-skel/hugo-go-template-without-whitespaces)

  ;; Indent setup
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-attr-indent-offset 2)
  (setq web-mode-comment-style 2)

  ;; Highlight current matched tags, and set its color
  (setq web-mode-enable-current-element-highlight t)
  (set-face-attribute 'web-mode-current-element-highlight-face nil :foreground "black" :background "PaleGreen2"))

;; Deactivate smartparens in `web-mode'
;; Using `web-mode' built-in auto-pairs and auto-quoting
(add-hook 'web-mode-hook #'spacemacs//deactivate-smartparens)

;; * `css-mode' setup
(with-eval-after-load 'css-mode
  (setq css-indent-offset 2))

;; * `js-mode' setup
(with-eval-after-load 'js-mode
  (setq js-indent-level 2))

;; * `typescript-mode' setup
(with-eval-after-load 'typescript-mode
  (setq typescript-indent-level 2))


(provide 'init-web)
;;; init-web.el ends here
