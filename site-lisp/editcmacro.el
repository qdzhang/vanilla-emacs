;;; editcmacro.el --- Edit C macro in a separate buffer. -*- lexical-binding: t; -*-

;; Copyright (C) 2018,2025  Anders Lindgren

;; Author: Anders Lindgren
;; Version: 0.0.1
;; Keywords: convenience, languages
;; Created: 2018-10-17
;; URL: https://github.com/Lindydancer/editcmacro
;; Package-Requires: ((emacs "24.3"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package lets you edit a C macro in a separate buffer, without
;; end of line backslashes.
;;
;; In C, macros are defined using the `#define' construct.
;; Technically, a C macro expand to a single line of tokens.  However,
;; for readability, a macro definition can be split into multiple
;; lines by using end of line backslashes.  For example:
;;
;;     #define FOREVER      \
;;       for (;;)           \
;;       {                  \
;;       }
;;
;; Unfortunately, the backslashes makes the code hard to edit.  This
;; package lets you edit a macro, without the end of line backslashes,
;; in a separate buffer.  When done, the macro is written back to the
;; original buffer, with backslashes.
;;
;; If you are familiar with source block editing in Org mode, you will
;; feel right at home with this package.

;; Keys:
;;
;; Press `C-c '' to edit a C macro in a separate buffer, without the
;; backslashes.  When done editing, you can press `C-c '' again to
;; write the macro back into the original buffer, with backslashes.
;;
;; You can discard your changes and kill the temporary buffer by
;; pressing `C-c C-k'.

;; Installation:
;;
;; Enable the minor mode `editcmacro-mode' in C-like buffers.
;; Typically, you can add the following to an appropriate init file:
;;
;;     (add-hook 'c-mode-hook   #'editcmacro-mode)
;;     (add-hook 'c++-mode-hook #'editcmacro-mode)

;;; Code:

;; Future ideas:
;;
;; - Preserve the mark.
;;
;; - Bind C-x C-s to something that write the macro back and save the
;;   original file (like org-src do).
;;
;; - Don't change "buffer modified" if the content hasn't changed.
;;
;; - Overlay in source buffer? (org-mode does this, but it's a lot of
;;   work.)
;;
;; - Warn when starting to edit a macro, if an edit session already is
;;   in progress.

;; Discussion:
;;
;; As the original indentation is lost in the temporary buffer, it's
;; impossible to tell apart the following:
;;
;;     #define NAME X  \
;;                  X
;;
;;     #define NAME X  \
;;       X
;;
;;     #define NAME X  \
;;     X
;;
;; The current solution is to indent according to `c-basic-offset',
;; which is how C mode typically indents multi-line macros.  An
;; alternative would be to remember the original indentation.

(require 'cc-mode)

(defvar editcmacro--buffer nil
  "Buffer of the original macro.")

(defvar editcmacro--beg nil
  "Beginning of original macro.")

(defvar editcmacro--body nil
  "Beginning of original macro body.")

(defvar editcmacro--end nil
  "End of original macro body.")

(defvar editcmacro--window-configuration nil
  "The window configuration before `editcmacro-edit-current-macro'.")


;; ------------------------------------------------------------
;; Support for retaining point
;;

;; The reason this function count number of characters from the end is
;; because the source and edit buffers are indented differently.

(defun editcmacro-line-end-position-without-ws-and-backslash ()
  "The end of line position, ignoring whitespace and end of line backslash."
  (save-excursion
    (end-of-line)
    (when (eq (char-before) ?\\)
      (forward-char -1))
    (skip-syntax-backward "-")
    (point)))


(defun editcmacro-coordinates (base p)
  "Return location of P relative to BASE.

Return (LINES . CHARS-FROM-EOL).  LINES is the nuber of lines
between BASE and P, where positive number mean that P is located
after BASE and negative numbers that it's located before.
CHARS-FROM-EOL COL is the number of characters from the end of
the line."
  (cons
   (- (line-number-at-pos p)
      (line-number-at-pos base))
   ;; In case 0 is returned, in case `p' is after the payload of the
   ;; line.
   (max (- (save-excursion
             (goto-char p)
             (editcmacro-line-end-position-without-ws-and-backslash))
           p)
        0)))


(defun editcmacro-goto-coordinates (base coordinates)
  "Goto COORDINATES, relative to BASE.

See `editcmacro-coordinates' for information on the format of
COORDINATES."
  (goto-char base)
  (when (eq (forward-line (car coordinates)) 0)
    (let ((dest (- (editcmacro-line-end-position-without-ws-and-backslash)
                   (cdr coordinates))))
      (goto-char (max dest (line-beginning-position))))))


;; ------------------------------------------------------------
;; Temporary edit buffer commands

(defun editcmacro-write-back ()
  "Write back the content of the edited macro to the original buffer."
  (interactive)
  (unless editcmacro--buffer
    (user-error "Expected to be in a *EditCMacro* buffer"))
  (unless (buffer-live-p editcmacro--buffer)
    (user-error "Original buffer is no more"))
  (let* ((orig-beg  editcmacro--beg)
         (orig-body editcmacro--body)
         (orig-end  editcmacro--end)
         ;; The end of the last line containing non-whitespace characters.
         (end (save-excursion
                (goto-char (point-max))
                (while (progn
                         (skip-syntax-backward "-")
                         (and (bolp)
                              (not (bobp))))
                  (forward-char -1))
                (point)))
         (coordinates (editcmacro-coordinates
                       end
                       (min
                        end
                        (point)))))
    (with-current-buffer editcmacro--buffer
      (goto-char orig-body)
      (delete-region orig-body orig-end))
    (save-excursion
      (goto-char (point-min))
      (let ((first t))
        ;; If the original macro was empty, ensure that there is at
        ;; least one space to separate the macro head from the body.
        (when (< (point) end)
          (with-current-buffer editcmacro--buffer
            (unless (or (bolp)
                        (looking-back "\\s-" (line-beginning-position)))
              (insert " "))))
        (while (< (point) end)
          (let ((s (buffer-substring-no-properties (line-beginning-position)
                                                   (line-end-position))))
            (with-current-buffer editcmacro--buffer
              (unless first
                (insert "\n"))
              (when (bolp)
                (insert (make-string
                         (if (and (boundp 'c-basic-offset)
                                  (integerp c-basic-offset))
                             c-basic-offset
                           2)
                         ?\s)))
              (insert s))
            (forward-line))
          (setq first nil))))
    (with-current-buffer editcmacro--buffer
      (editcmacro-goto-coordinates orig-end coordinates)
      (save-excursion
        (c-backslash-region orig-beg orig-end nil)))))


(defun editcmacro-done ()
  "Write back the edited macro and kill the temporary buffer."
  (interactive)
  (editcmacro-write-back)
  (editcmacro-kill-and-restore))


(defun editcmacro-kill-and-restore ()
  "Kill the temporary buffer and restore the window configuration."
  (unless editcmacro--buffer
    (user-error "Expected to be in a *EditCMacro* buffer"))
  (let ((buf editcmacro--buffer)
        (edit-buffer (current-buffer)))
    (set-buffer buf)
    (set-window-configuration editcmacro--window-configuration)
    ;; Nore: For some reason (unknown to me), the point isn't retained
    ;; when the edit buffer is killed before the window configuration
    ;; is restored.
    (kill-buffer edit-buffer)
    (setq editcmacro--window-configuration nil)))


(defun editcmacro-abort ()
  "Abort editing the macro.

This will kill the temporary buffer."
  (interactive)
  (editcmacro-kill-and-restore))


;; ------------------------------------------------------------
;; Source buffer commands
;;

(defun editcmacro-expect-and-skip (regexp msg)
  "If point is at REGEXP, goto the end, otherwise issue MSG."
  (if (looking-at regexp)
      (goto-char (match-end 0))
    (user-error msg)))

(defun editcmacro-edit-current-macro ()
  "Edit the C macro at the point in a temporary buffer.

In the temporary buffer, end of line backslashes (used in
multi-line macros) are removed.  When done, the macro is written
back to the source buffer with backslashes.

In the temporary edit buffer the following keys can be used:
\\{editcmacro-edit-mode-map}"
  (interactive)
  (save-excursion
    (let* ((beg
            (save-excursion
              (beginning-of-line)
              (while (eq (char-before (- (point) 1)) ?\\)
                (forward-line -1))
              (point)))
           (body
            (save-excursion
              (goto-char beg)
              (editcmacro-expect-and-skip "\\s-*#\\s-*define\\>\\s-*"
                                          "Not in macro definition")
              (when (eolp)
                (user-error "Macro name missing"))
              ;; Skip name
              (forward-sexp)
              ;; Skip parameter list, if present.
              (when (eq (char-after) ?\()
                (forward-sexp))
              (skip-syntax-forward "-")
              (when (looking-at "\\\\$")
                (forward-line))
              (point)))
           (end
            (save-excursion
              (end-of-line)
              (while (eq (char-before) ?\\)
                (forward-line)
                (end-of-line))
              (point)))
           (point-coordinates (editcmacro-coordinates end (point))))
      ;; --------------------
      ;; Find the leftmost column contanining anything except
      ;; whilespace.
      ;;
      ;; Note: First line may start on same line as the #define, as is:
      ;;
      ;; #define X 1 \
      ;;           + 2
      ;;
      ;; In this case, this should find column 10.
      (let ((min-column nil))
        (save-excursion
          (goto-char body)
          (while (< (point) end)
            (skip-syntax-forward "-")
            (when (or (null min-column)
                      (< (current-column) min-column))
              (setq min-column (current-column)))
            (forward-line)))
        ;; --------------------
        ;; Copy macro content to other buffer.
        (let ((edit-buffer (get-buffer-create "*EditCMacro*")))
          (with-current-buffer edit-buffer
            (erase-buffer))
          (goto-char body)
          (if (and min-column
                   (< (current-column) min-column))
              (move-to-column min-column))
          (while
              (progn
                (let ((s (buffer-substring-no-properties
                          (point)
                          (save-excursion
                            (end-of-line)
                            (when (eq (char-before) ?\\)
                              (backward-char)
                              (skip-syntax-backward "-"))
                            (point)))))
                  (with-current-buffer edit-buffer
                    (insert s)
                    (insert "\n")))
                (forward-line)
                (< (point) end))
            (move-to-column min-column))
          ;; Record information about original macro and start the
          ;; appropriate major mode.
          (let ((mode major-mode)
                (orig-buf (current-buffer)))
            ;; Markers are used because:
            ;;
            ;; - It allows the user to edit the original buffer
            ;;   while editing a macro.
            ;;
            ;; - It makes it possible to run `editcmacro-done'
            ;;   multiple times, without deleting too much the
            ;;   second time.
            ;;
            ;; Note: Original buffer must be current.
            (setq body (copy-marker body nil))
            (setq end (copy-marker end t))
            (with-current-buffer edit-buffer
              (funcall mode)
              (editcmacro-edit-mode 1)
              (setq-local editcmacro--buffer orig-buf)
              (setq-local editcmacro--beg  beg)
              (setq-local editcmacro--body body)
              (setq-local editcmacro--end  end)
              (setq-local
               header-line-format
               (substitute-command-keys
	        "Edit, then exit with `\\[editcmacro-done]' or abort with \
`\\[editcmacro-abort]'"))))
          (setq editcmacro--window-configuration
                (current-window-configuration))
          (select-window (display-buffer edit-buffer))
          (editcmacro-goto-coordinates
           (max (point-min)
                (- (point-max) 1))
           point-coordinates)
          ;; Return edit buffer.
          edit-buffer)))))


;; ------------------------------------------------------------
;; Minor modes
;;

;; --------------------
;; Minor mode enabled in the source buffers, but not in *EditCMacro* buffers.

(defvar editcmacro-src-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c '") #'editcmacro-edit-current-macro)
    map)
  "Keymap used in the source buffer when Editcmacro mode is enabled.")

(define-minor-mode editcmacro-src-mode
  "Support mode for `editcmacro-mode'.

This mode is enabled in source buffers, but not the temporary
edit buffer."
  :keymap editcmacro-src-mode-map)


;; --------------------
;; Minor mode enabled in the *EditCMacro* buffer.

(defvar editcmacro-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c '")   #'editcmacro-done)
    (define-key map (kbd "C-c C-k") #'editcmacro-abort)
    map)
  "Keymap used in the temporary buffer when editing a C macro.")

(define-minor-mode editcmacro-edit-mode
  "Support mode for `editcmacro-mode'.

This mode is enabled in the temporary buffer where the C macro is
being edited."
  :keymap editcmacro-edit-mode-map
  ;; Disable the source mode, to ensure it's binding doesn't take
  ;; precedence.
  (editcmacro-src-mode -1))


;; --------------------
;; User-level minor mode.
;;
;; This is intended to be added to relevant major mode hooks.

;; Note: This is enabled in the major mode hook used both in the
;; source and temporary buffers.  To avoid a potential key binding
;; collision, this mode enables the suppport modes
;; `editcmacro-src-mode' in the source mode and `editcmacro-edit-mode'
;; in the temporary buffer.  The support modes hold the keymap.
;;
;; This should work regardless of the order in which `editcmacro-mode'
;; and `editcmacro-edit-mode' are enabled.

;;;###autoload
(define-minor-mode editcmacro-mode
  "Minor mode to support editing C macros in a separate buffer.

The following keys are used:
\\{editcmacro-src-mode-map}"
  :keymap nil
  (unless editcmacro-edit-mode
    (editcmacro-src-mode 1)))


;; ------------------------------------------------------------
;; The end
;;

(provide 'editcmacro)

;;; editcmacro.el ends here
