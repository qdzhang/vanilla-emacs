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


(setq org-default-notes-file "~/org/inbox.org"
      org-agenda-files '("~/org/task.org" "~/org/refile.org" "~/org/archive.org")
      org-todo-keywords '((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)"))
      ;; org-ellipsis " ??? "
      ;; org-startup-indented t
      org-log-into-drawer "LOGBOOK"
      org-log-done 'time
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

(defun my/org-find-month-in-datetree()
  "Use this function to capture datetree only for month, not day
Ref:https://emacs.stackexchange.com/a/58326"
  (org-datetree-find-date-create (calendar-current-date))
  (kill-line))

(setq org-capture-templates
      `(("t" "Work Task" entry
         (file+headline "~/org/refile.org" "Tasks")
         "* TODO %^{?????????} %^g\n%U\n" :empty-lines 1)
        ("b" "Reading Books" entry
         (file+headline "~/org/books.org" "Books")
         "** IN-PROGRESS %^{Book name} %^g\n:PROPERTIES:\n:start-at: %U\n:end-at:\n:END:\n%?" :empty-lines 1)
        ("i" "Inbox" entry
         (file+headline "~/org/refile.org" "Inbox")
         "* %U - %^{heading} %^g\n%?" :empty-lines 1)
        ("r" "Weekly Review" plain
         (file+function "~/org/review.org" my/org-find-month-in-datetree)
         "*** %U Weekly review\n%?" :empty-lines 1)
        ("d" "Daily Target" checkitem
         (file+olp+datetree "~/org/daily.org")
         "- [ ] %?" :prepend t :kill-buffer t)
        ("s" "New snippet" entry
         (file+headline "~/org/snippets.org" "Code snippets")
         "* %^{??????????????????} %^g\n:PROPERTIES:\n:time: %U\n:origin: %^{????????????}\n:describes: %?\n:END:\n\n#+begin_src\n \n#+end_src\n" :empty-lines 1)
        ("j" "Journal" entry
         (file+olp+datetree "~/org/journal.org")
         "* %?\n%U\n" :clock-in t :empty-lines 1)
        ("n" "Notes" entry
         (file+headline "~/org/notes.org" "Notes")
         "** %U - %^{heading} %^g\n%?" :empty-lines 1)))

(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9)
                                 ("~/org/notes.org" :maxlevel . 9))))

;; Make week starts on Monday
(setq calendar-week-start-day 1)

;; Make `org-clock-report' contains all levels of subtree
(setq org-clock-clocktable-default-properties '(:maxlevel 9))

;; Use full outline paths for refile targets
(setq org-refile-use-outline-path t)

;; Targets complete directly with fido
(setq org-outline-path-complete-in-steps nil)

;; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

(add-hook 'org-mode-hook
          (lambda ()
            (require 'init-flymake)
            (add-hook 'flymake-diagnostic-functions 'flymake-vale nil t)))


(set-register ?t (cons 'file "~/org/task.org"))
(set-register ?j (cons 'file "~/org/journal.org"))
(set-register ?b (cons 'file "~/org/books.org"))
(set-register ?d (cons 'file "~/org/daily.org"))
(set-register ?r (cons 'file "~/org/refile.org"))
(set-register ?a (cons 'file "~/org/archive.org"))
(set-register ?s (cons 'file "~/org/snippets.org"))

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
