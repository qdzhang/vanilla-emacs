;;; init-isearch.el --- isearch config               -*- lexical-binding: t; -*-

(setq isearch-lazy-count t
      lazy-count-prefix-format "%s/%s "
      isearch-allow-motion t
      search-whitespace-regexp "[-_ \t\n]+")

(defun my/toggle-search-whitespace ()
  "Set `search-whitespace-regexp' to nil or includes hyphen lowline tab newline.
Explanation: When in isearch (M-x `isearch-forward'), space key can also stand
for other chars such as hyphen lowline tab newline. It depend on a regex. It's
convenient. But sometimes you want literal. This command makes it easy to toggle.

Emacs Isearch Space Toggle
URL `http://xahlee.info/emacs/emacs/emacs_isearch_space.html'
Version 2019-02-22 2021-11-13"
  (interactive)
  (if (string-equal search-whitespace-regexp nil)
      (progn
        (setq search-whitespace-regexp "[-_ \t\n]+")
        (message "Space set to hyphen lowline tab newline space"))
    (progn
      (setq search-whitespace-regexp nil)
      (message "Space set to literal."))))

(define-key isearch-mode-map (kbd "<up>") 'isearch-ring-retreat)
(define-key isearch-mode-map (kbd "<down>") 'isearch-ring-advance)

(define-key isearch-mode-map (kbd "<left>") 'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "<right>") 'isearch-repeat-forward)

;; https://karthinks.com/software/it-bears-repeating/
;; This post provide a comprehensive introduction of `repeat-mode'.
;; The following isearch configuration is an example in this post.
(defvar isearch-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s") #'isearch-repeat-forward)
    (define-key map (kbd "r") #'isearch-repeat-backward)
    map))

(dolist (cmd '(isearch-repeat-forward isearch-repeat-backward))
  (put cmd 'repeat-map 'isearch-repeat-map))

;; Use `phi-search' to replace with `isearch'
(with-eval-after-load 'phi-search
  (define-key phi-search-default-map (kbd "<left>") 'phi-search-again-or-previous)
  (define-key phi-search-default-map (kbd "<right>") 'phi-search-again-or-next))

(provide 'init-isearch)
