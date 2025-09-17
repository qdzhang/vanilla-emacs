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

  ;; Define a custom function to close and indent web tags easily
  (defun web-mode-element-close-and-indent ()
    (interactive)
    (unless (char-equal ?> (preceding-char))
      (insert ">"))
    (web-mode-element-close)
    (indent-for-tab-command))

  (define-key web-mode-map (kbd "C-c /") 'web-mode-element-close-and-indent)

  (setq web-mode-extra-snippets
        '(("erb" . (("if" . "<% if | %>\n\n<% end %>")
                    ("end" . "|\n<% end %>")
                    ("render" . "<%= render | %>")
                    ("link" . "<%= link_to | %>")
                    ("paginate" . "<%= will_paginate | %>")
                    ("form" . "<%= form_with(model: |) do %>\n\n<% end %>")
                    ("provide" . "<% provide(|) %>")
                    ("yield" . "<%= yield| %>")
                    ("debug" . "<%= debug(params) if Rails.env.development? %>")
                    ("img" . "<%= image_tag \"|\" %>")))
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
          ("erb" . "\\.rhtml\\'")
          ("mojolicious" . "\\.ep\\'")))

  (setq web-mode-extra-auto-pairs
        '(("erb"  . (("beg" "end")))
          ("php"  . (("beg" "end")
                     ("beg" "end"))))))


(defun my/web-mode-for-django ()
  (when (project-current)
    (let ((pr (project-root (project-current))))
      (when pr
        (if (file-exists-p (concat pr "manage.py"))
            (progn
              (web-mode-set-engine "django")
              (message "Determine Django project: Setting web template to Django"))
          (message "Not a Django project"))))))

(add-hook 'web-mode-hook 'my/web-mode-for-django)


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
(defun my/typescript-mode-hook ()
  "`typescript-mode' hook function."
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (setq-local
               eldoc-documentation-strategy
               'eldoc-documentation-compose)))
  (eglot-ensure)
  (subword-mode 1))

(with-eval-after-load 'typescript-ts-mode
  (require 'init-eglot))
(with-eval-after-load 'tsx-ts-mode
  (require 'init-eglot))
(add-hook 'typescript-ts-mode-hook 'my/typescript-mode-hook)
(add-hook 'tsx-ts-mode-hook (lambda ()
                              (eglot-ensure)))


(defun node-repl ()
  (interactive)
  (setenv "NODE_NO_READLINE" "1")  ; Avoid fancy terminal codes
  (setq comint-prompt-read-only t)
  (inheritenv
   (pop-to-buffer (make-comint "node-repl" "node" nil "--interactive"))))


;; Config `astro-mode'
(setq web-mode-enable-front-matter-block t)

(provide 'init-web)
;;; init-web.el ends here
