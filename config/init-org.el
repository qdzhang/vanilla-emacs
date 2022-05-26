;;; init-org.el --- Config org-mode                  -*- lexical-binding: t; -*-

;;; Commentary:

;; Config org-mode

;;; Code:

(defun my/org-font-setup ()
  "Setup variable-pitch fonts for `org-mode'."
  (interactive)
  (variable-pitch-mode 1)
  (text-scale-increase 1.2)

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
   `(org-block ((t (:inherit fixed-pitch :background "#f8f8f8"))))
   `(org-block-begin-line ((t (:background "#e0e0e0" :extend t))))
   `(org-block-end-line ((t (:background "#e0e0e0" :extend t))))
   `(org-document-info ((t (:foreground "dark orange"))))
   `(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
   `(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   `(org-property-value ((t (:inherit fixed-pitch))) t)
   `(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   `(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
   `(org-verbatim ((t (:inherit (shadow fixed-pitch)))))))

(add-hook 'org-mode-hook #'my/org-font-setup)


(setq org-default-notes-file "~/org/notes.org"
      org-agenda-files '("~/org/agenda.org")
      org-todo-keywords '((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)"))
      ;; org-ellipsis "  "
      ;; org-startup-indented t
      org-log-into-drawer "LOGBOOK"
      org-archive-location "~/org/archive.org::datetree/"
      org-src-fontify-natively t

      ;; edit in current window
      org-src-window-setup 'current-window

      ;; do not put two spaces on the left
      org-src-preserve-indentation nil
      org-src-tab-acts-natively t
      org-edit-src-content-indentation 0)

;; Config org-capture-templates
(setq org-capture-templates nil)
(add-to-list 'org-capture-templates '("t" "Tasks"))
(add-to-list 'org-capture-templates
             '("tr" "Book Reading Task" entry
               (file+olp "~/org/task.org" "Reading" "Book")
               "* TODO %^{书名}\n%u\n%a\n" :clock-in t :clock-resume t))
(add-to-list 'org-capture-templates
             '("tw" "Work Task" entry
               (file+headline "~/org/task.org" "Work")
               "* TODO %^{任务名}\n%u\n%a\n" :clock-in t :clock-resume t))
(add-to-list 'org-capture-templates
             '("i" "Inbox" entry (file "~/org/inbox.org")
               "* %U - %^{heading} %^g\n %?\n"))
(add-to-list 'org-capture-templates
             '("s" "New snippet" entry
               (file+headline "~/org/snippets.org" "Code snippets")
               "* %^{代码片段描述} %^g\n:PROPERTIES:\n:time: %U\n:origin: %^{代码来源}\n:describes: %?\n:END:\n\n#+begin_src\n \n#+end_src\n" :empty-lines 1))

(provide 'init-org)
;;; init-org.el ends here
