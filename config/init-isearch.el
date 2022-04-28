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

(provide 'init-isearch)
