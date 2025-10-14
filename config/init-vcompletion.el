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


;; Sort by frequency
;; Reference:
;; https://robbmann.io/posts/emacs-29-completions/
(defun renz/sort-by-alpha-length (elems)
  "Sort ELEMS first alphabetically, then by length."
  (sort elems (lambda (c1 c2)
                (or (string-version-lessp c1 c2)
                    (< (length c1) (length c2))))))

(defun renz/sort-by-history (elems)
  "Sort ELEMS by minibuffer history.
Use `mct-sort-sort-by-alpha-length' if no history is available."
  (if-let ((hist (and (not (eq minibuffer-history-variable t))
                      (symbol-value minibuffer-history-variable))))
      (minibuffer--sort-by-position hist elems)
    (renz/sort-by-alpha-length elems)))

(defun renz/completion-category ()
  "Return completion category."
  (when-let ((window (active-minibuffer-window)))
    (with-current-buffer (window-buffer window)
      (completion-metadata-get
       (completion-metadata (buffer-substring-no-properties
                             (minibuffer-prompt-end)
                             (max (minibuffer-prompt-end) (point)))
                            minibuffer-completion-table
                            minibuffer-completion-predicate)
       'category))))

(defun renz/sort-multi-category (elems)
  "Sort ELEMS per completion category."
  (pcase (renz/completion-category)
    ('nil elems) ; no sorting
    ('kill-ring elems)
    ('project-file (renz/sort-by-alpha-length elems))
    (_ (renz/sort-by-history elems))))

(setq completions-sort #'renz/sort-multi-category)


(define-key completion-in-region-mode-map (kbd "M-n") 'switch-to-completions)

;; Select completion list item in normal buffer
(define-key completion-in-region-mode-map (kbd "C-n") 'minibuffer-next-completion)
(define-key completion-in-region-mode-map (kbd "C-p") 'minibuffer-previous-completion)

;; Jump from completions buffer to minibuffer
;; `M-v': Jump from minibuffer to completions buffer
(define-key completion-list-mode-map (kbd "v") #'switch-to-minibuffer)

;; Select completion list item in minibuffer without get into `completion-list-mode'
(define-key minibuffer-local-map (kbd "C-n") 'minibuffer-next-completion) ;; emacs29
(define-key minibuffer-local-map (kbd "C-p") 'minibuffer-previous-completion) ;; emacs29
(define-key minibuffer-local-map (kbd "M-RET") 'minibuffer-choose-completion) ;; emacs29

(provide 'init-vcompletion)
;;; init-vcompletion.el ends here
