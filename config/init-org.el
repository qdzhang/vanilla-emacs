;;; init-org.el --- Config org-mode                  -*- lexical-binding: t; -*-

;;; Commentary:

;; Config org-mode

;;; Code:

(defun my/org-font-setup ()
  "Setup variable-pitch fonts for `org-mode'."
  (interactive)
  (variable-pitch-mode 1)
  (auto-fill-mode)

  (custom-theme-set-faces
   'user
   `(org-level-1 ((t (:inherit (outline-1 fixed-pitch) :weight bold))))
   `(org-level-2 ((t (:inherit (outline-2 fixed-pitch) :weight bold))))
   `(org-level-3 ((t (:inherit (outline-3 fixed-pitch) :weight bold))))
   `(org-level-4 ((t (:inherit (outline-4 fixed-pitch) :weight bold))))
   `(org-level-5 ((t (:inherit (outline-5 fixed-pitch) :weight bold))))
   `(org-level-6 ((t (:inherit (outline-6 fixed-pitch) :weight bold))))
   `(org-level-7 ((t (:inherit (outline-7 fixed-pitch) :weight bold))))
   `(org-level-8 ((t (:inherit (outline-8 fixed-pitch) :weight bold))))
   `(org-table ((t (:inherit fixed-pitch))))
   `(org-formula ((t (:inherit fixed-pitch))))
   `(org-code ((t (:inherit fixed-pitch :foreground "#0f7f5f"))))
   `(org-footnote ((t (:inherit (org-link fixed-pitch)))))
   `(org-ellipsis ((t (:underline nil :foreground "dark gray"))))
   `(org-drawer ((t (:foreground "dark gray"))))
   `(org-block ((t (:inherit fixed-pitch :height 0.9))))
   `(org-block-begin-line
     ((t (:box (:style release-button) :slant italic :foreground "dark gray"))))
   `(org-block-end-line
     ((t (:box (:style release-button) :slant italic :foreground "dark gray"))))
   `(org-document-info ((t (:foreground "dark orange"))))
   `(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
   `(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   `(org-property-value ((t (:inherit fixed-pitch))) t)
   `(org-date ((t (:inherit (link fixed-pitch)))) t)
   `(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   ;; `(org-tag ((t (:box (:line-width 1)))))
   `(org-checkbox ((t :inherit 'fixed-pitch-serif :background unspecified :box unspecified)))
   `(org-verbatim ((t (:inherit (shadow fixed-pitch) :foreground "#ba2d2f"))))))

(add-hook 'org-mode-hook #'my/org-font-setup)



(defun my/writing-mode ()
  "A simple funciton for distraction-free writing."
  (interactive)
  (setq-local visual-fill-column-center-text t)
  (setq-local visual-fill-column-width 50)
  (auto-fill-mode -1)
  (save-excursion
    (set-mark (point-min))
    (goto-char (point-max))
    (my/unfill-paragraph-or-region)
    (deactivate-mark))
  (visual-line-mode 1)
  (visual-fill-column-mode 1))

(setq org-default-notes-file "~/org/default.org"
      org-agenda-files '("~/org/todo.org" "~/org/done.org")
      org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "SOMEDAY(s)" "|" "DONE(d)" "CANCELED(c)"))
      org-ellipsis " ▼"
      ;; org-startup-indented t
      org-log-into-drawer "LOGBOOK"
      org-clock-into-drawer "CLOCK"
      org-log-done 'time
      org-archive-location "~/org/archive.org::datetree/"
      org-src-fontify-natively t
      org-fontify-quote-and-verse-blocks t

      ;; edit in current window
      org-src-window-setup 'current-window

      ;; do not put two spaces on the left
      org-src-preserve-indentation nil
      org-src-tab-acts-natively t
      ;; do not show code highlight in src blocks
      org-src-fontify-natively nil
      org-edit-src-content-indentation 0)

(setq org-todo-keyword-faces
      '(("NEXT" . (:foreground "dodger blue" :weight bold))
        ("SOMEDAY" . (:foreground "DarkOrange" :weight bold))
        ("CANCELED" . (:foreground "darkgray"))))


;; Config org-capture-templates
(defun my/org-find-month-in-datetree()
  "Use this function to capture datetree only for month, not day
Ref:https://emacs.stackexchange.com/a/58326"
  (org-datetree-find-date-create (calendar-current-date))
  (kill-line))

(setq org-capture-templates
      `(("t" "Work Task" entry
         (file "~/org/todo.org")
         "* TODO %^{任务名} %^g\n%U\n" :empty-lines 1)
        ("r" "Weekly Review" plain
         (file+function "~/org/review.org" my/org-find-month-in-datetree)
         "*** %U Weekly review\n%?" :empty-lines 1)
        ("s" "New snippet" entry
         (file+headline "~/org/snippets/snippets.org" "Code snippets")
         "* %^{代码片段描述} %^g\n:PROPERTIES:\n:time: %U\n:origin: %^{代码来源}\n:describes: %?\n:END:\n\n#+begin_src\n \n#+end_src\n" :empty-lines 1)
        ("j" "Journal" entry
         (file+olp+datetree "~/org/journal.org")
         "* %?\n%U\n" :clock-in t :empty-lines 1)
        ("w" "Whim" entry
         (file+olp+datetree "~/org/whim.org")
         "* %?\n%U\n" :empty-lines 1)
        ("f" "Fun websites" entry
         (file+headline "~/org/fun-web.org" "Websites")
         "* %^{heading} %^g\n:PROPERTIES:\n:time: %U\n:url: %^{url}\n:describes: %?\n:other: \n:END:\n" :empty-lines 1)
        ))

(setq org-refile-targets '((org-agenda-files :maxlevel . 3)
                           ("~/org/done.org" :maxlevel . 3)))

;; Make week starts on Monday
(setq calendar-week-start-day 1)

;; Make `org-clock-report' contains all levels of subtree
(setq org-clock-clocktable-default-properties '(:maxlevel 3))

;; See the details about refile in https://blog.aaronbieber.com/2017/03/19/organizing-notes-with-refile.html
;;
;; Use full outline paths for refile targets
;; Nodes can be refiled to the top level of the file
(setq org-refile-use-outline-path 'file)

;; Targets complete directly with fido
(setq org-outline-path-complete-in-steps nil)

;; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes 'confirm)

;; Sublevels are intented with leading dots in `org-tags-view'(C-c a m)
(setq org-tags-match-list-sublevels 'indented)


(add-hook 'org-agenda-mode-hook (lambda () (hl-line-mode 1)))

(defun my/org-align-all-tables ()
  (interactive)
  (org-table-map-tables 'org-table-align 'quietly))

(advice-add 'org-agenda-quit :before 'org-save-all-org-buffers)

(defun my/org-align-all-tags ()
  (interactive)
  (org-align-tags t))

(with-eval-after-load 'org-attach
  (setq org-attach-store-link-p t))

(with-eval-after-load 'org-agenda
  (add-to-list 'org-modules 'org-habit)
  (require 'org-habit)

  (add-to-list 'org-agenda-custom-commands
               '("2" "Next todo"
                 todo "NEXT"
                 ((org-agenda-max-entries 5)))
               t)

  (defun my/org-agenda-goto ()
    "A custom `org-agenda-goto' to narrow to the item's subtree"
    (interactive)
    (when (equal major-mode 'org-agenda-mode)
      (org-agenda-goto)
      (org-narrow-to-subtree)))

  (define-key org-agenda-mode-map (kbd "<tab>") 'my/org-agenda-goto))

(with-eval-after-load 'org-habit
  (setq org-habit-show-all-today t))


(with-eval-after-load 'org
  (defun my-org//heading-current-file ()
    "Return current file for use with `transient' heading."
    buffer-file-name)

  (transient-define-prefix my-transient//org-hyperlink ()
    "Org Links"
    [["Navigate"
      ("g" "Follow Link" org-open-at-point)
      ("n" "Next Link" org-next-link)
      ("p" "Previous Link" org-previous-link)
      ("r" "Occur Links" org-occur-link-in-agenda-files)]
     ["Manage"
      ("l" "Store Link" org-store-link)
      ("i" "Insert Link" org-insert-link)
      ("t" "Toggle Link Display" org-toggle-link-display)]]
    [:hide (lambda () t)
           ("s" org-store-link)])

  (transient-define-prefix my-transient//org-time ()
    "Time"
    [["Insert"
      ("t" "Timestamp" org-time-stamp)
      ("T" "Inactive Timestamp" org-time-stamp-inactive)
      ("s" "Schedule Item" org-schedule)
      ("d" "Deadline" org-deadline)
      ("c" "Goto Calendar" org-goto-calendar)
      ("C" "Date from Calendar" org-date-from-calendar)]
     ["Timer"
      ("0" "Start Timer" org-timer-start)
      ("9" "Stop Timer" org-timer-stop)
      ("8" "Pause/Continue Timer" org-timer-pause-or-continue)
      ("7" "Insert Timer String" org-timer)
      ("6" "Insert Timer Item" org-timer-item)]
     ["Manage"
      ("D" "Change Date..." matcha-org-change-date)
      ("y" "Evaluate Time Range" org-evaluate-time-range)
      ("Z" "Custom Time Format" org-toggle-time-stamp-overlays)]])

  (transient-define-prefix matcha-org-change-date ()
    "Change Date"
    ["Change Date"
     ("l" "1 Day Later" org-shiftright)
     ("h" "1 Day Before" org-shiftleft)
     ("k" "1 ... Later" org-shiftup)
     ("j" "1 ... Before" org-shiftdown)])

  (transient-define-prefix my-transient//org-editing ()
    "Edit"
    [
     :description
     (lambda () (propertize
                 (format "Org: %s" (my-org//heading-current-file))
                 'face 'org-level-1))
     ["Insert"
      ("m" "Heading" org-meta-return)
      ("M" "Heading Under" org-insert-heading-respect-content)
      ("t" "Todo" org-insert-todo-heading)
      ("T" "Todo Under" org-insert-todo-heading-respect-content)]
     ["Promotion"
      ("<left>" "Promoto Heading" org-do-promote)
      ("<right>" "Demote Heading" org-do-demote)
      ("S-<left>" "Promote Subtree" org-promote-subtree)
      ("S-<right>" "Demote Subtree" org-demote-subtree)
      ("<up>" "Move Subtree Up" org-move-subtree-up)
      ("<down>" "Move Subtree Down" org-move-subtree-down)]
     ["Mark"
      ("e" "Element" org-mark-element :transient t)
      ("@" "Subtree" org-mark-subtree :transient t)]]
    [["Subtree"
      ("x" "Cut" org-cut-subtree)
      ("w" "Copy" org-copy-subtree)
      ("y" "Paste" org-paste-subtree)
      ("Y" "Yank" org-yank)
      ("W" "Clone" org-clone-subtree-with-time-shift)]
     ["Modify"
      ("r" "Refile" org-refile)
      ("^" "Sort" org-sort)
      ("*" "Toggle Heading" org-toggle-heading)]
     ["Narrow"
      ("ns" "Narrow to Subtree" org-narrow-to-subtree)
      ("nb" "Narrow to Block" org-narrow-to-block)
      ("nw" "Widen" widen)]])

  (transient-define-prefix my-transient//org-clock ()
    "Org clock transient menu"
    [["Control"
      ("i" "In" org-clock-in)
      ("o" "Out" org-clock-out)
      ("j" "Jump" org-clock-goto)]
     ["Show"
      ("r" "Report" org-clock-report)
      ("d" "Display" org-clock-display)
      ("e" "Effort" org-clock-modify-effort-estimate)]])

  (transient-define-prefix my-transient/org-mode-menu ()
    "Org Mode"
    [
     :description
     (lambda () (propertize
                 (format "Org: %s" (my-org//heading-current-file))
                 'face 'org-level-1))
     ["Motion"
      ("n" "Next Heading" org-next-visible-heading :transient t)
      ("p" "Previous Heading" org-previous-visible-heading :transient t)
      ("f" "Forward Level" org-forward-heading-same-level :transient t)
      ("b" "Backward Level" org-backward-heading-same-level :transient t)
      ("u" "Up Heading" outline-up-heading :transient t)
      ("j" "Goto" org-goto)]
     ["Misc"
      ("e" "Editing..." my-transient//org-editing)
      ("t" "Time..." my-transient//org-time)
      ("l" "Links..." my-transient//org-hyperlink)
      ("c" "Clock" my-transient//org-clock)
      ("r" "Reveal" org-reveal)]]))


(provide 'init-org)
;;; init-org.el ends here
