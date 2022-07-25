;;; init-smartparens.el --- Init for smartparens  -*- lexical-binding: t; -*-

(require 'smartparens-config)

(setq show-smartparens-global-mode nil)
(setq show-smartparens-mode nil)
(add-hook 'prog-mode-hook #'smartparens-strict-mode)

(transient-define-prefix my-transient/smartparens-menu ()
  "Smartparens transient menu"
  [["Move"
    ("d" "Down" sp-down-sexp)
    ("e" "Up" sp-up-sexp)
    ("u" "Backward Up" sp-backward-up-sexp)
    ("a" "Backward Down" sp-backward-down-sexp)
    ("f" "Forward" sp-forward-sexp)
    ("b" "Backward" sp-backward-sexp)]
   ["Change"
    ("k" "Kill" sp-kill-sexp)
    ("r" "Raise" sp-raise-sexp)
    ("t" "Transpose" sp-transpose-sexp)
    ("c" "Convolute" sp-convolute-sexp)
    ("j" "Join" sp-join-sexp)]
   ["Wrap"
    ("(" "Wrap ()" sp-wrap-round)
    ("{" "Wrap {}" sp-wrap-curly)
    ("[" "Wrap []" sp-wrap-square)]])

(defun my/sp-new-line ()
  "This is my version of `sp-newline'.
Jump out a sexp, then call `sp-newline'"
  (interactive)
  (sp-up-sexp)
  (sp-newline))

(defun my/sp-kill-word (arg)
  "Kill characters forward until encountering the end of a word, or the current selection.
Copied from: https://christiantietze.de/posts/2020/05/delete-word-or-region-emacs/"
  (interactive "p")
  (if (use-region-p)
      (delete-active-region 'kill)
    (sp-kill-word arg)))

(with-eval-after-load 'web-mode
  (sp-local-pair 'web-mode "<" nil :actions :rem))

(with-eval-after-load 'js-mode
  (sp-local-pair 'js-mode "<" nil :actions :rem))

;; Fix the conflict of smartparens and electric-pairs in `cperl-mode'
;; https://github.com/syl20bnr/spacemacs/issues/480#issuecomment-262340062
(with-eval-after-load 'cperl-mode
  (add-hook 'smartparens-enabled-hook  (lambda () (define-key cperl-mode-map "{" nil)))
  (add-hook 'smartparens-disabled-hook  (lambda () (define-key cperl-mode-map "{" 'cperl-electric-lbrace))))

(defvar sp-splurp-barf-sexp-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<right>") #'sp-forward-slurp-sexp)
    (define-key map (kbd "<left>") #'sp-forward-barf-sexp)
    map)
  "Keymap to repeat `sp-forward-barf-sexp' and `sp-forward-slurp-sexp'")

(dolist (cmd '(sp-forward-slurp-sexp sp-forward-barf-sexp))
  (put cmd 'repeat-map 'sp-splurp-barf-sexp-repeat-map))

(defvar sp-movement-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "f") '("forward-sexp" . sp-forward-sexp))
    (define-key map (kbd "b") '("backward-sexp" . sp-backward-sexp))
    (define-key map (kbd "d") '("down-sexp" . sp-down-sexp))
    (define-key map (kbd "u") '("up-sexp" . sp-up-sexp))
    map)
  "Keymap to quick move across sexp.")

(dolist (cmd '(sp-forward-sexp sp-backward-sexp sp-down-sexp sp-up-sexp))
  (put cmd 'repeat-map 'sp-movement-repeat-map))

(provide 'init-smartparens)
