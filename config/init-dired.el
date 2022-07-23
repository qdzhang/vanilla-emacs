;;; init-dired.el --- dired-config                   -*- lexical-binding: t; -*-


(add-hook 'dired-mode-hook 'dired-hide-details-mode)
(add-hook 'dired-mode-hook 'auto-revert-mode)

(setq dired-dwim-target t)
(setq dired-guess-shell-alist-user '(("\\.pdf\\'" "zathura")
                                     ("\\.djvu\\'" "zathura")
                                     ("\\.mkv\\'"  "mpv")
                                     ("\\.avi\\'"  "mpv")
                                     ("\\.mp4\\'"  "mpv")
                                     ("\\.m4v\\'"  "mpv")
                                     ("\\.flv\\'"  "mpv")
                                     ("\\.wmv\\'"  "mpv")
                                     ("\\.mpg\\'"  "mpv")
                                     ("\\.mpeg\\'" "mpv")
                                     ("\\.webm\\'" "mpv")
                                     ("\\.mp3\\'" "mpv --profile=builtin-pseudo-gui")
                                     ("\\.ape\\'" "mpv --profile=builtin-pseudo-gui")
                                     ("\\.flac\\'" "mpv --profile=builtin-pseudo-gui")
                                     ("\\.webp\\'" "vwebp")
                                     ("\\.jpg\\'" "pqiv")
                                     ("\\.png\\'" "pqiv")
                                     ("\\.gif\\'" "pqiv")
                                     ("\\.jpeg\\'" "pqiv")
                                     ("\\.epub\\'" "zathura")
                                     ("\\.azw3\\'" "ebook-viewer")))
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)
(setq dired-isearch-filenames 'dwim)
(setq delete-by-moving-to-trash t)
(setq dired-listing-switches "-AFhlv --group-directories-first")

;; Avoid popup `Async Shell Command' window when using `dired-do-async-shell-command'
;; https://emacs.stackexchange.com/questions/5553/async-shell-process-buffer-always-clobbers-window-arrangement
(add-to-list 'display-buffer-alist (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil)))

(with-eval-after-load 'dired
  (require 'dired-x)
  (require 'dired-aux)

  (add-to-list 'dired-compress-file-suffixes
               '("\\.rar\\'" ".rar" "unrar x")))


(defvar dired-filelist-cmd
  '(("mpv")
    ("zathura")))

;; Create a new procedure to start a process in dired without popup windows.
;; The process will persist when Emacs is closed.
;; https://emacs.stackexchange.com/a/5558
(defun my/dired-start-process (cmd &optional file-list)
  (interactive
   (let ((files (dired-get-marked-files t current-prefix-arg)))
     (list
      (dired-read-shell-command "& on %s: " current-prefix-arg files)
      files)))
  (start-process
   cmd nil shell-file-name
   shell-command-switch
   (format "nohup 1>/dev/null 2>/dev/null %s \"%s\""
           (if (> (length file-list) 1)
               (format "%s %s" cmd
                       (cadr (assoc cmd dired-filelist-cmd)))
             cmd)
           (mapconcat #'expand-file-name file-list "\" \""))))

(defun my/dired-filter ()
  "Dired show filtered files"
  (interactive)
  (call-interactively #'dired-mark-files-regexp)
  (progn (dired-toggle-marks)
         (dired-do-kill-lines)))

(defun my/xah-dired-sort ()
  "Sort dired dir listing in different ways.
Prompt for a choice.
URL `http://ergoemacs.org/emacs/dired_sort.html'
Version 2018-12-23"
  (interactive)
  (let ($sort-by $arg)
    (setq $sort-by (ido-completing-read "Sort by:" '( "date" "size" "name" )))
    (cond
     ((equal $sort-by "name") (setq $arg "-Al "))
     ((equal $sort-by "date") (setq $arg "-Al -t"))
     ((equal $sort-by "size") (setq $arg "-Al -S"))
     ;; ((equal $sort-by "dir") (setq $arg "-Al --group-directories-first"))
     (t (error "logic error 09535" )))
    (dired-sort-other $arg )))

;; https://oremacs.com/2015/01/12/dired-file-size/
(defun my/dired-get-size ()
  (interactive)
  (let ((files (dired-get-marked-files)))
    (with-temp-buffer
      (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
      (message
       "Size of all marked files: %s"
       (progn
         (re-search-backward "\\(^[ 0-9.,]+[A-Za-z]+\\).*total$")
         (match-string 1))))))

(defun my/browse-marked-file ()
  "Open the marked file in dired as a URL using `browse-url'."
  (interactive)
  (let ((marked-files (dired-get-marked-files nil)))
    (dolist (file-name marked-files)
      (if (and (fboundp 'tramp-tramp-file-p)
               (tramp-tramp-file-p file-name))
          (error "Cannot open tramp file")
        (browse-url (concat "file://" file-name))))))

(defun my/dired-find-all-marked-files (&optional arg)
  "Open each of the marked files, or the file under the point, or when prefix arg, the next N files "
  (interactive "P")
  (let* ((fn-list (dired-get-marked-files nil arg)))
    (mapc 'find-file fn-list)))

;; Ediff marked files in dired
;; https://oremacs.com/2017/03/18/dired-ediff/
(defun my/ediff-files ()
  (interactive)
  (let ((files (dired-get-marked-files))
        (wnd (current-window-configuration)))
    (if (<= (length files) 2)
        (let ((file1 (car files))
              (file2 (if (cdr files)
                         (cadr files)
                       (read-file-name
                        "file: "
                        (dired-dwim-target-directory)))))
          (if (file-newer-than-file-p file1 file2)
              (ediff-files file2 file1)
            (ediff-files file1 file2))
          (add-hook 'ediff-after-quit-hook-internal
                    (lambda ()
                      (setq ediff-after-quit-hook-internal nil)
                      (set-window-configuration wnd))))
      (error "no more than 2 files should be marked"))))

(defun my/terminal-here ()
  "Open an external terminal from Emacs in `default-directory'"
  (interactive)
  (start-process "terminal" nil "urxvt"))

(transient-define-prefix my-transient/dired-menu ()
  "Dires transient menu"
  [("b" "Open in browser" my/browse-marked-file)
   ("e" "Ediff-files" my/ediff-files)
   ("i" "Image dired" image-dired)
   ("s" "Dired sort" my/xah-dired-sort)
   ("t" "Terminal here" my/terminal-here)
   ("z" "File size" my/dired-get-size)])


(provide 'init-dired)
