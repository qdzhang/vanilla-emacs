;;; init-vcompletion.el --- Config vcomplete mode    -*- lexical-binding: t; -*-

;;; Commentary:

;; Config built-in completions buffer and vcomplete package
;;
;; References:
;; - https://robbmann.io/posts/emacs-29-completions/
;; - https://www.scss.tcd.ie/~sulimanm/posts/default-emacs-completion.html

;;; Code:

;; * Config built-in completions buffer
;; ** Hide mode-line in completions buffer
(add-hook 'completion-list-mode-hook 'hidden-mode-line-mode)

;; ** Ignore case
(setq completion-ignore-case t)
(setq pcomplete-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)

;; ** Show detailed info in completions buffer
(setq completions-detailed nil)

;; ** Completions buffer styles
(setq completions-format 'one-column)
(setq completion-show-help nil)
(setq completion-show-inline-help nil)
(when (> emacs-major-version 28)
  (setq completion-auto-help 'always)
  (setq completion-auto-select 'second-tab))


;; ** fuzzy completing
(add-to-list 'completion-styles 'flex t)

;; * Config `vcomplete-mode'
;; (require 'vcomplete)
;; (vcomplete-mode)

;; ** Config position and size of completions buffer
(if (> emacs-major-version 28)
    (setq completions-max-height 10)
  (add-to-list 'display-buffer-alist
               '("\\*Completions\\*"
                 (display-buffer-reuse-window display-buffer-in-side-window)
                 (side . bottom)
                 (window-height . 0.33)
                 (slot . 0))))

;; live update for completions
;; Reference:
;; https://emacs-china.org/t/emacs-28-1-fido-vertical-mode/20474/5
(defun live-completions--update (&rest _)
  "Update the *Completions* buffer.
Meant to be added to `after-change-functions'."
  (when (minibufferp) ; skip if we've exited already
    (let ((while-no-input-ignore-events '(selection-request)))
      (while-no-input
        (condition-case nil
            (save-match-data
              (save-excursion
                (goto-char (point-max))
                (let ((inhibit-message t)
                      (ring-bell-function #'ignore))
                  (minibuffer-completion-help))))
          (quit (abort-recursive-edit)))))))


(defun live-completions--setup ()
  "Setup live updating for the *Completions* buffer.
Meant to be added to `minibuffer-setup-hook'."
  (unless (memq (or (bound-and-true-p current-minibuffer-command) this-command)
                '(execute-extended-command describe-command describe-symbol
                                           describe-function describe-variable))
    (add-hook 'after-change-functions #'live-completions--update nil t)))

(add-hook 'minibuffer-setup-hook #'live-completions--setup)

(define-key completion-in-region-mode-map (kbd "M-n") #'switch-to-completions)

;; Jump from completions buffer to minibuffer
;; `M-v': Jump from minibuffer to completions buffer
(define-key completion-list-mode-map (kbd "e") #'switch-to-minibuffer)

;; Select completion list item without get into `completion-list-mode'
(define-key minibuffer-mode-map (kbd "C-n") #'minibuffer-next-completion) ;; emacs29
(define-key minibuffer-mode-map (kbd "C-p") #'minibuffer-previous-completion) ;; emacs29
(define-key minibuffer-mode-map (kbd "M-RET") #'minibuffer-choose-completion) ;; emacs29

(provide 'init-vcompletion)
;;; init-vcompletion.el ends here
