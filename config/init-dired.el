;;; init-dired.el --- dired-config                   -*- lexical-binding: t; -*-


(add-hook 'dired-mode-hook 'dired-hide-details-mode)
(add-hook 'dired-mode-hook 'auto-revert-mode)

(setq dired-dwim-target t)
(setq dired-guess-shell-alist-user '(("\\.pdf\\'" "mupdf")
                                     ("\\.djvu\\'" "djview")
                                     ("\\.mkv\\'"  "mpv")
                                     ("\\.avi\\'"  "mpv")
                                     ("\\.mp4\\'"  "mpv")
                                     ("\\.m4v\\'"  "mpv")
                                     ("\\.flv\\'"  "mpv")
                                     ("\\.rmvb\\'"  "mpv")
                                     ("\\.wmv\\'"  "mpv")
                                     ("\\.mpg\\'"  "mpv")
                                     ("\\.mpeg\\'" "mpv")
                                     ("\\.webm\\'" "mpv")
                                     ("\\.mp3\\'" "mpv --profile=builtin-pseudo-gui")
                                     ("\\.ape\\'" "mpv --profile=builtin-pseudo-gui")
                                     ("\\.flac\\'" "mpv --profile=builtin-pseudo-gui")
                                     ("\\.m4a\\'" "mpv --profile=builtin-pseudo-gui")
                                     ("\\.webp\\'" "xnviewmp")
                                     ("\\.jpg\\'" "xnviewmp")
                                     ("\\.png\\'" "xnviewmp")
                                     ("\\.gif\\'" "xnviewmp")
                                     ("\\.jpeg\\'" "xnviewmp")
                                     ("\\.svg\\'" "xnviewmp")
                                     ("\\.epub\\'" "mupdf")
                                     ("\\.azw3\\'" "ebook-viewer")))
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)
(setq dired-isearch-filenames 'dwim)
(setq delete-by-moving-to-trash t)
(setq dired-listing-switches "-AFhlv --group-directories-first")

(with-eval-after-load 'dired
  (require 'dired-x)
  (require 'dired-aux)

  (add-to-list 'dired-compress-file-suffixes
               '("\\.rar\\'" "" "unrar x %i %o"))

  (setq dired-compress-files-alist
        '(("\\.tar\\.gz\\'" . "tar -cf - %i | gzip -c9 > %o")
          ("\\.tar\\.bz2\\'" . "tar -cf - %i | bzip2 -c9 > %o")
          ("\\.tar\\.xz\\'" . "tar -cf - %i | xz -c9 > %o")
          ("\\.tar\\.zst\\'" . "tar -cf - %i | zstd -19 -o %o")
          ("\\.tar\\.lz\\'" . "tar -cf - %i | lzip -c9 > %o")
          ("\\.tar\\.lzo\\'" . "tar -cf - %i | lzop -c9 > %o")
          ;; ("\\.zip\\'" . "zip %o -r --filesync %i")
          ("\\.zip\\'" . "7z a -tzip %o %i")
          ("\\.7z\\'" . "7z a %o %i")
          ("\\.rar\\'" . "rar a %o %i")
          ("\\.pax\\'" . "pax -wf %o %i"))))



(defvar dired-filelist-cmd
  '(("mpv")
    ("zathura")
    ("djview")
    ("mupdf")))

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
  (start-process "terminal" nil "lxterminal"))

(defun my/dired-parent-directory ()
  "Open parent directory in current buffer"
  (interactive)
  (find-alternate-file ".."))

(transient-define-prefix my-transient/dired-help-menu ()
  "Show dired help menu"
  [
   :description
   (lambda ()
     (propertize "Dired Help Menu" 'face 'warning))
   ["Directory"
    ("+" "New directory" dired-create-directory)
    ("g" "Revert buffer" revert-buffer)        ;; read all directories again (refresh)
    ("w" "Kill subdir" dired-kill-subdir)
    ("l" "redisplay" dired-do-redisplay)   ;; relist the marked or singel directory
    ("k" "Kill lines" dired-do-kill-lines)
    ("i" "Insert subdir" dired-maybe-insert-subdir)
    ("_" "Undo" dired-undo)]
   ["Immediate"
    ("a" "Parent dir" my/dired-parent-directory)
    ("f" "Visit file" dired-find-file)
    ("o" "Visit file(other window)" dired-find-file-other-window)
    ("O" "Display" dired-display-file)
    ("v" "View" dired-view-file)      ;; q to exit, s to search, = gets line #
    ("s" "Sort by..." my/xah-dired-sort)]
   ["Flag for Deletion"
    ("~" "Flag backup files" dired-flag-backup-files)
    ("d" "Flag for deletion" dired-flag-file-deletion)
    ("#" "Flag autosave files" dired-flag-auto-save-files)
    ("%d" "Flag matching regexp" dired-flag-files-regexp)
    ("%." "Flag extension" dired-flag-extension)
    ("%^" "Flag garbage files" dired-flag-garbage-files)]]
  [["Mark"
    ("m" "mark" dired-mark)
    ("u" "unmark" dired-unmark)
    ("U" "unmark all" dired-unmark-all-marks)
    ("*." "mark extension" dired-mark-extension)
    ("*/" "mark all directories" dired-mark-directories)
    ("*@" "mark all symlinks" dired-mark-symlinks)
    ("**" "mark all executables" dired-mark-executables)
    ("*s" "mark subdir files" dired-mark-subdir-files)
    ("*%" "mark matching regexp" dired-mark-files-regexp)
    ("*c" "change marks" dired-change-marks)
    ("F" "find marked" dired-do-find-marked-files)
    ("t" "invert mark" dired-toggle-marks)
    ("{" "prev marked" dired-prev-marked-file)
    ("}" "next marked" dired-next-marked-file)]
   ["Actions on marks"
    ("C" "Copy to" dired-do-copy)        ;; Copy all marked files
    ("R" "Rename to" dired-do-rename)
    ("D" "Delete" dired-do-delete)
    ("S" "Symlink to" dired-do-symlink)
    ("Y" "Relative symlink to" dired-do-relsymlink)
    ("Z" "Compress" dired-do-compress)
    ("c" "Compress to..." dired-do-compress-to)
    ("G" "Change group" dired-do-chgrp)
    ("M" "Change mode" dired-do-chmod)
    ("A" "Find regexp" dired-do-find-regexp)
    ("Q" "Find regexp and replace" dired-do-find-regexp-and-replace)
    ("E" "Ediff" my/ediff-files)
    ("%l" "Lower case" dired-downcase)
    ("%u" "Upper case" dired-upcase)]
   ["Info"
    ("(" "Hide details" dired-hide-details-mode)
    (")" "dired-omit-mode" dired-omit-mode)
    ("?" "Dired summary" dired-summary)
    ("z" "File size" my/dired-get-size)]
   ["External"
    ("!" "shell" dired-smart-shell-command)
    ("&" "async shell" async-shell-command)
    ("b" "Open in browser" my/browse-marked-file)
    ("T" "Open terminal" my/terminal-here)
    ("e" "EWW" eww-open-file)
    ("q" "Quit" keyboard-quit)]])

;;;###autoload
(defun my/find-file-fd ()
  "Use `fd' to find files in current directory, and open it in dired."
  (interactive)
  (let* ((command (format "fd -H -t f --strip-cwd-prefix -0"))
         (cands (split-string (shell-command-to-string command) "\0" t))
         (file (completing-read "Fd file: " cands)))
    (when file
      (dired-jump nil file))))


(provide 'init-dired)
