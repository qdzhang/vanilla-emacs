;;; init-edit.el -*- lexical-binding: t; -*-

(require 'mark-thing-at)
(require 'selected)

(dolist (mode '(prog-mode-hook text-mode-hook))
  (add-hook mode 'selected-minor-mode))
(define-key selected-keymap (kbd "q") #'selected-off)
(define-key selected-keymap (kbd "U") #'upcase-region)
(define-key selected-keymap (kbd "D") #'downcase-region)
(define-key selected-keymap (kbd "C") #'capitalize-dwim)
(define-key selected-keymap (kbd "M") #'apply-macro-to-region-lines)
(define-key selected-keymap (kbd "a") #'my/query-replace-append)
(define-key selected-keymap (kbd "A") #'my/query-replace-add-prefix)
(define-key selected-keymap (kbd "%") #'my/query-replace-selected)
(define-key selected-keymap (kbd "o") #'browse-url)

;; Define some keybindings to use `mark-thing-at' functions.
;; There is no need to use `mark-thing-at-mode'. Defining keybindings in
;; `selected-minor-mode' has shorter keystrokes and better consistency.
(define-key selected-keymap (kbd "c") #'mark-sentence)
(define-key selected-keymap (kbd "d") #'mark-defun-thing)
(define-key selected-keymap (kbd "e") #'mark-sexp-thing)
(define-key selected-keymap (kbd "f") #'mark-filename)
(define-key selected-keymap (kbd "n") #'my/mark-defun-name)
(define-key selected-keymap (kbd "h") #'mark-whitespace)
(define-key selected-keymap (kbd "t") #'mark-list)
(define-key selected-keymap (kbd "l") #'mark-line-this)
(define-key selected-keymap (kbd "w") #'mark-word-thing)
(define-key selected-keymap (kbd "u") #'mark-url)
(define-key selected-keymap (kbd "s") #'mark-symbol)

(defun my/query-replace-in-selected-mode (pos)
  "Generic query-replace in `selected-mode' to operate the selected region.
POS is a symbol indicates the position which replace will operate. The value are
'self, 'prefix and 'suffix."
  (save-excursion
    (when (use-region-p)
      (when (= (point) (region-end))
        (exchange-point-and-mark))
      (let* ((selected-content (buffer-substring (region-beginning)
                                                 (region-end)))
             (to-string (cond
                         ((eq pos 'self)
                          (read-string "Replace with: "))
                         ((eq pos 'prefix)
                          (concat (read-string "Add prefix to selected: ")
                                  selected-content))
                         ((eq pos 'suffix)
                          (concat selected-content
                                  (read-string "Append to selected: ")))
                         (t (message "Wrong position to operate the replacement")))))
        (replace-regexp selected-content to-string)))))

(defun my/query-replace-selected ()
  "Replace current selected region with something."
  (interactive)
  (my/query-replace-in-selected-mode 'self))

(defun my/query-replace-append ()
  "Append something to currrent selected region."
  (interactive)
  (my/query-replace-in-selected-mode 'suffix))

(defun my/query-replace-add-prefix ()
  "Add some prefix to current selected region."
  (interactive)
  (my/query-replace-in-selected-mode 'prefix))

;;;###autoload
(defun my/smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there.

URL: https://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/"
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `my/smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'my/smarter-move-beginning-of-line)

;;;###autoload
(defun my/sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file.

URL: https://emacsredux.com/blog/2013/04/21/edit-files-as-root/"
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defalias 'rb 'revert-buffer)
(defalias 'ba 'borg-assimilate)
(defalias 'bb 'borg-build)
(defalias 'bd 'borg-remove)


;; unfill paragraph: the opposite of `fill-paragraph'
;;;###autoload
(defun my/unfill-paragraph-or-region (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

;;; Cycle Letter Case
;;; https://gist.github.com/abo-abo/29f581cac2f615fd709d
;;;###autoload
(defun my/capitalize-word-toggle ()
  (interactive)
  (let ((start (car
                (save-excursion
                  (backward-word)
                  (bounds-of-thing-at-point 'symbol)))))
    (if start
        (save-excursion
          (goto-char start)
          (funcall
           (if (char-upcasep (char-after))
               'downcase-region
             'upcase-region)
           start (1+ start)))
      (capitalize-word -1))))

;;;###autoload
(defun my/upcase-word-toggle ()
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'symbol))
        beg end
        regionp)
    (if (eq this-command last-command)
        (setq regionp (get this-command 'regionp))
      (put this-command 'regionp nil))
    (cond
     ((or (region-active-p) regionp)
      (setq beg (region-beginning)
            end (region-end))
      (put this-command 'regionp t))
     (bounds
      (setq beg (car bounds)
            end (cdr bounds)))
     (t
      (setq beg (point)
            end (1+ beg))))
    (save-excursion
      (goto-char (1- beg))
      (and (re-search-forward "[A-Za-z]" end t)
           (funcall (if (char-upcasep (char-before))
                        'downcase-region
                      'upcase-region)
                    beg end)))))

(defun char-upcasep (letter)
  (eq letter (upcase letter)))

;;;###autoload
(defun my/increment-number-at-point ()
  (interactive)
  (skip-chars-backward "0-9")
  (or (looking-at "[0-9]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

;;;###autoload
(defun my/time-stamp ()
  "Insert time stamp"
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))


;; Open newline
;; https://github.com/manateelazycat/open-newline/blob/master/open-newline.el
;;;###autoload
(defun my/open-newline-above (arg)
  "Move to the previous line (like vi) and then opens a line."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (if (derived-mode-p 'prog-mode)
      (indent-according-to-mode)
    (beginning-of-line)))

;;;###autoload
(defun my/open-newline-below (arg)
  "Move to the next line (like vi) and then opens a line."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (call-interactively 'next-line arg)
  (if (derived-mode-p 'prog-mode)
      (indent-according-to-mode)
    (beginning-of-line)))

;;;###autoload
(defun my/rename-file-and-buffer ()
  "Renames the current buffer and the file it is visiting.

URL: https://whhone.com/emacs-config/#rename-file-and-buffer-together"
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

;;;###autoload
(defun my/delete-file-and-buffer ()
  "Kills the current buffer and deletes the file it is visiting.

URL: https://whhone.com/emacs-config/#delete-file-and-buffer-together"
  (interactive)
  (let ((filename (buffer-file-name)))
    (if filename
        (if (y-or-n-p (concat "Do you really want to delete file " filename " ?"))
            (progn
              (delete-file filename)
              (message "Deleted file %s." filename)
              (kill-buffer)))
      (message "Not a file visiting buffer!"))))

;;;###autoload
(defun my/get-filename ()
  "Copy the full path of the current buffer.
If current buffer is not associated with a file, show a warning."
  (interactive)
  (if-let (file-name (buffer-file-name
                      (window-buffer (minibuffer-selected-window))))
      (progn
        (kill-new file-name)
        (message "%s" file-name))
    (message "WARNING: Current buffer is not associated with a file!")))

;;;###autoload
(defun my/get-buffername ()
  "Copy and show the name of the current buffer."
  (interactive)
  (message (kill-new (buffer-name))))

;;;###autoload
(defun my/get-directory-path ()
  "Copy and show the directory path of the current buffer.
If the buffer is not associated with a file, use the `list-buffers-directory'
variable as a fallback to display the directory, useful in buffers like the ones
created by `magit' and `dired'."
  (interactive)
  (if-let (directory-path (if-let (file-name (buffer-file-name))
                              (file-name-directory file-name)
                            list-buffers-directory))
      (progn
        (kill-new directory-path)
        (message "%s" directory-path))
    (message "WARNING: Current buffer does not have a directory!")))


(defun my/mark-defun-name ()
  "Mark the current function name. If not in a function body, hold on the
position and message a warning.
This procedure also works on `defmacro', `defface' and scheme `define'."
  (interactive)
  (let ((old-pos (point)))
    (save-restriction
      (narrow-to-defun)
      (goto-char (point-min))
      (if (or (search-forward-regexp "defun\\|defmacro\\|defface" nil t)
              (search-forward-regexp "define (*" nil t))
          (progn
            (forward-word)
            (mark-sexp-thing))
        (deactivate-mark)
        (goto-char old-pos)
        (message "Not in a function body!")))))


(provide 'init-edit)
