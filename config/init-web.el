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
  (set-face-attribute 'web-mode-current-element-highlight-face nil :foreground "black" :background "PaleGreen2")

  (setq web-mode-extra-snippets
        '(("erb" . (("toto" . "<% toto | %>\n\n<% end %>")
                    ("form" . "<% form_with(model: |, local: true) do %>\n\n<% end %>")))
          ))

  ;; `C-c C-m' to mark, and press `m' to expand
  (defvar web-mode-mark-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "m") #'web-mode-mark-and-expand)
      map))

  (dolist (cmd '(web-mode-mark-and-expand))
    (put cmd 'repeat-map 'web-mode-mark-repeat-map))

  (setq web-mode-engines-alist
        '(("php" . "\\.phtml\\'")
          ("erb" . "\\.erb\\.")
          ("erb" . "\\.rhtml\\'"))))

;; Deactivate smartparens in `web-mode'
;; Using `web-mode' built-in auto-pairs and auto-quoting
(add-hook 'web-mode-hook #'spacemacs//deactivate-smartparens)

;; * `css-mode' setup
(with-eval-after-load 'css-mode
  (setq css-indent-offset 2))

;; * `js-mode' setup
(with-eval-after-load 'js
  (setq js-indent-level 2))

;; * `typescript-mode' setup
(with-eval-after-load 'typescript-mode
  (setq typescript-indent-level 2))

(defun my/typescript-mode-hook ()
  "`typescript-mode' hook function."
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (setq-local
               eldoc-documentation-strategy
               'eldoc-documentation-compose)))
  (subword-mode 1))

(add-hook 'typescript-mode-hook 'my/typescript-mode-hook)

(provide 'init-web)
;;; init-web.el ends here
