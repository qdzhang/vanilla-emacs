;;; init-edit.el -*- lexical-binding: t; -*-

(require 'selected)
(require 'thingatpt)

(defun my/mark-things (thing)
  "A generic function to mark things follow `thing-at-point'."
  (let ((bounds (bounds-of-thing-at-point thing)))
    (when (null bounds)
      (error "Unkown thing."))
    (goto-char (car bounds))
    (push-mark nil t t)
    (goto-char (cdr bounds))))

;; Define `line-this' thing for `thing-at-point'
(put 'line-this 'beginning-op #'(lambda () (beginning-of-line)))
(put 'line-this 'end-op #'(lambda () (end-of-line)))

(defvar my/mark-things-list
  '(symbol list sexp defun filename url email uuid word sentence whitespace
           line line-this page)
  "The THINGS used in `my/mark-things-gen'.")

(defmacro my/mark-things-gen (thing)
  "Generate functions that mark things actually."
  `(defun ,(intern (concat "my/mark-" (symbol-name thing))) ()
     ,(format "Mark %s at point." thing)
     (interactive)
     (my/mark-things ',thing)))

;; Generate all `my/mark-THING' like functions
(dolist (thing my/mark-things-list)
  (eval
   `(my/mark-things-gen ,thing)))

;; Keybindings for `selected-minor-mode'
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

;; Define some keybindings to use `thing-at-point' functions.
;; Defining keybindings in `selected-minor-mode' has shorter keystrokes and
;; better consistency.
(define-key selected-keymap (kbd "c") #'my/mark-sentence)
(define-key selected-keymap (kbd "d") #'my/mark-defun)
(define-key selected-keymap (kbd "e") #'my/mark-sexp)
(define-key selected-keymap (kbd "f") #'my/mark-filename)
(define-key selected-keymap (kbd "n") #'my/mark-defun-name)
(define-key selected-keymap (kbd "h") #'my/mark-whitespace)
(define-key selected-keymap (kbd "t") #'my/mark-list)
(define-key selected-keymap (kbd "l") #'my/mark-line-this)
(define-key selected-keymap (kbd "w") #'my/mark-word)
(define-key selected-keymap (kbd "u") #'my/mark-url)
(define-key selected-keymap (kbd "s") #'my/mark-symbol)

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
  (push (point) buffer-undo-list)
  (beginning-of-line)
  (open-line arg)
  (if (derived-mode-p 'prog-mode)
      (indent-according-to-mode)
    (beginning-of-line)))

;;;###autoload
(defun my/open-newline-below (arg)
  "Move to the next line (like vi) and then opens a line."
  (interactive "p")
  (push (point) buffer-undo-list)
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
      (if (or (search-forward-regexp "defun\\|defmacro\\|defface\\|defcustom" nil t)
              (search-forward-regexp "define (*" nil t))
          (progn
            (forward-word)
            (my/mark-sexp))
        (deactivate-mark)
        (goto-char old-pos)
        (message "Not in a function body!")))))

;;; Config `pulsar' package
(require 'pulsar)

;; Check the default value of `pulsar-pulse-functions'.  That is where
;; you add more commands that should cause a pulse after they are
;; invoked
(add-to-list 'pulsar-pulse-functions 'tab-bar-switch-to-tab)
(add-to-list 'pulsar-pulse-functions 'project-switch-project)
(add-to-list 'pulsar-pulse-functions 'my/quick-window-jump)

(setq pulsar-pulse t)
(setq pulsar-delay 0.06)
(setq pulsar-iterations 10)
(setq pulsar-face 'pulsar-green)
(setq pulsar-highlight-face 'pulsar-yellow)

(pulsar-global-mode 1)

;; pulsar does not define any key bindings.  This is just a sample that
;; respects the key binding conventions.  Evaluate:
;;
;;     (info "(elisp) Key Binding Conventions")
;;
;; The author uses C-x l for `pulsar-pulse-line' and C-x L for
;; `pulsar-highlight-line'.
;;
;; You can replace `pulsar-highlight-line' with the command
;; `pulsar-highlight-dwim'.
(let ((map global-map))
  (define-key map (kbd "C-c h p") #'pulsar-pulse-line)
  (define-key map (kbd "C-c h h") #'pulsar-highlight-line))


;;;###autoload
(defun my/kill-ring-save ()
  "Call `kill-ring-save' on region. If there is no region, call it on the whole
line.

TODO: `duplicate-dwim' is added in Emacs 29. Maybe rewrite this function when
Emacs 29 releases."
  (interactive)
  (if mark-active
      (kill-ring-save (region-beginning) (region-end))
    (progn
      (kill-ring-save (line-beginning-position) (line-end-position))
      (message "The whole line is copied"))))


;; Shift selected region
;; ref: https://github.com/claasz/emacs/blob/master/dot_emacs_xw6600
(defun my/shift-region (distance)
  (let ((mark (mark)))
    (save-excursion
      (indent-rigidly (region-beginning) (region-end) distance)
      (push-mark mark t t)
      ;; Tell the command loop not to deactivate the mark
      ;; for transient mark mode
      (setq deactivate-mark nil))))

;; Shift selected region right
(defun my/shift-right ()
  (interactive)
  (my/shift-region 2))

;; Shift selected region left
(defun my/shift-left ()
  (interactive)
  (my/shift-region -2))

(provide 'init-edit)
