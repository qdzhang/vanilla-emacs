;;; init-edit.el -*- lexical-binding: t; -*-

(require 'mark-thing-at)
(mark-thing-at-mode)

(require 'selected)
(dolist (mode '(prog-mode-hook text-mode-hook))
  (add-hook mode 'selected-minor-mode))
(define-key selected-keymap (kbd "q") #'selected-off)
(define-key selected-keymap (kbd "u") #'upcase-region)
(define-key selected-keymap (kbd "d") #'downcase-region)
(define-key selected-keymap (kbd "c") #'capitalize-dwim)
(define-key selected-keymap (kbd "m") #'apply-macro-to-region-lines)

;; Define some keybindings to use `mark-thing-at' functions
(define-key selected-keymap (kbd "e") #'mark-sexp-thing)
(define-key selected-keymap (kbd "t") #'mark-list)
(define-key selected-keymap (kbd "l") #'mark-line-this)
(define-key selected-keymap (kbd "w") #'mark-word-thing)


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
  (if (not (member major-mode '(haskell-mode org-mode literate-haskell-mode)))
      (indent-according-to-mode)
    (beginning-of-line)))

;;;###autoload
(defun my/open-newline-below (arg)
  "Move to the next line (like vi) and then opens a line."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (call-interactively 'next-line arg)
  (if (not (member major-mode '(haskell-mode org-mode literate-haskell-mode)))
      (indent-according-to-mode)
    (beginning-of-line)))

(provide 'init-edit)
