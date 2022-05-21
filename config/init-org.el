;;; init-org.el --- Config org-mode                  -*- lexical-binding: t; -*-

;;; Commentary:

;; Config org-mode

;;; Code:

(defun my/org-font-setup ()
  "Setup variable-pitch fonts for `org-mode'."
  (interactive)
  (variable-pitch-mode)

  (custom-theme-set-faces
   'user
   `(org-level-1 ((t (:inherit (outline-1 variable-pitch) :height 1.5 :weight bold))))
   `(org-level-2 ((t (:inherit (outline-2 variable-pitch) :height 1.4 :weight bold))))
   `(org-level-3 ((t (:inherit (outline-3 variable-pitch) :height 1.3 :weight bold))))
   `(org-level-4 ((t (:inherit (outline-4 variable-pitch) :height 1.2 :weight bold))))
   `(org-level-5 ((t (:inherit (outline-5 variable-pitch) :height 1.1 :weight bold))))
   `(org-level-6 ((t (:inherit (outline-6 variable-pitch) :height 1.1 :weight bold))))
   `(org-level-7 ((t (:inherit (outline-7 variable-pitch) :height 1.1 :weight bold))))
   `(org-level-8 ((t (:inherit (outline-8 variable-pitch) :height 1.1 :weight bold))))
   `(org-table ((t (:inherit fixed-pitch))))
   `(org-formula ((t (:inherit fixed-pitch))))
   `(org-code ((t (:inherit fixed-pitch))))
   `(org-footnote ((t (:inherit (org-link fixed-pitch)))))
   `(org-block ((t (:inherit fixed-pitch))))
   `(org-document-info ((t (:foreground "dark orange"))))
   `(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
   `(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   `(org-property-value ((t (:inherit fixed-pitch))) t)
   `(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   `(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
   `(org-verbatim ((t (:inherit (shadow fixed-pitch)))))))

(add-hook 'org-mode-hook #'my/org-font-setup)

(provide 'init-org)
;;; init-org.el ends here
