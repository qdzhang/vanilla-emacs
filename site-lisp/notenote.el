;;; notenote.el --- A simple note-taking system using pure org-mode  -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Leverage the core functionality of Org-mode, for effective note-taking.
;; Users of Emacs can harness this built-in feature to create a robust and simple
;; note-taking system.
;;
;; Some useful vanilla org-mode keybindings:
;;
;; - =C-c C-l=: `org-insert-link'. Insert a link. With =C-u= prefix to
;;   insert a file link specially.
;; - =C-c l=: `org-store-link'. Store a link to current location and can be
;;   inserted into other Org buffer with `org-insert-link'
;; - =C-c C-o=: `org-open-at-point'. Open a link at point.
;; - =C-c &=: `org-mark-ring-goto'. Jump back to a recorded position.
;;
;; `org-toggle-link-display': Toggle the literal or descriptive display of links.
;; If you want to edit a link, use the literal link will be convenient.

;; https://gist.github.com/redblobgames/3ef970bdeeef0e4a025d2981ce83ed27

;;; Code:

(require 'org)
(require 'org-agenda)
(require 'transient)

(defvar notenote-directory (concat org-directory "/TIL"))

(defun notenote-title-to-filename (title)
  "Convert TITLE to a reasonable filename."
  (let* ((time-sec (format-time-string "%3N%s"))
         (random-num (format "%.5s" time-sec))
         (date (format-time-string "%Y-%m-%d")))
    (setq title
          (thread-last
            title
            (downcase)
            (replace-regexp-in-string "[^a-zA-Z0-9]+" "-")
            (replace-regexp-in-string "-+" "-")
            (replace-regexp-in-string "^-" "")
            (replace-regexp-in-string "-$" "")
            (concat date "-" random-num "-"))))
  title)


(defun notenote-new (title)
  "Create a new note given a title."
  (interactive "sTitle: ")
  (let ((default-directory (concat notenote-directory "/")))
    (find-file (concat (notenote-title-to-filename title) ".org"))
    (when (= 0 (buffer-size))
      (insert "#+title: " title "\n"
              "#+date: ")
      (org-insert-time-stamp nil t)
      (insert "\n#+filetags:")
      (insert "\n\n"))))

(defun notenote-open ()
  "Open an existing note."
  (interactive)
  (let ((default-directory (concat notenote-directory "/")))
    (call-interactively 'find-file)))

(defun notenote-directory ()
  "Open dired with all notes files."
  (interactive)
  (dired notenote-directory "-lt"))

(defun notenote-get-id ()
  "Generate a unique ID for an entry, and get it."
  (interactive)
  (org-id-copy))

;; Use `org-agenda' to search for the tags in all notes
(defun notenote-tags-search ()
  "Search tags in all notes."
  (interactive)
  (let ((org-agenda-files (file-expand-wildcards (concat notenote-directory "/*.org"))))
    (org-tags-view)))

(defun notenote-search (regexp)
  "Search all notes in regexp"
  (interactive "sRegexp: ")
  (rgrep regexp "*.org" (concat notenote-directory "/")))

(defun notenote-todo-list ()
  "Show all todo list in notes."
  (interactive)
  (let ((org-agenda-files (file-expand-wildcards (concat notenote-directory "/*.org"))))
    (org-todo-list)))


(transient-define-prefix notenote-menu ()
  "A transient menu for notenote."
  [
   :description
   (lambda ()
     (propertize "\nNoteNote!\n" 'face 'bold-italic))
   ["Note"
    ("n" "new" notenote-new)
    ("o" "open" notenote-open)
    ("d" "directory" notenote-directory)]
   ["Search"
    ("t" "search tags" notenote-tags-search)
    ("s" "search regexp" notenote-search)
    ("<" "todo list" notenote-todo-list)]
   ["Edit"
    ("i" "copy ID" notenote-get-id)]])

(provide 'notenote)
;;; notenote.el
