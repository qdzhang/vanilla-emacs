;;; init-smartparens.el --- Init for smartparens  -*- lexical-binding: t; -*-

(require 'smartparens-config)

(setq show-smartparens-global-mode nil)
(setq show-smartparens-mode nil)
(add-hook 'prog-mode-hook #'smartparens-strict-mode)

(with-eval-after-load 'smartparens
  (setq sp-highlight-pair-overlay nil))

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
    ("[" "Wrap []" sp-wrap-square)]
   ["Select"
    ("n" "Select next" sp-select-next-thing)
    ("p" "Select previous" sp-select-previous-thing)
    ("N" "Select next (hold point)" sp-select-next-thing-exchange)
    ("P" "Select previous (hold point)" sp-select-previous-thing-exchange)]])

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

;; After curly brace, enter `RET' will create an extra newline
;; https://emacs.stackexchange.com/questions/12368/make-ending-curly-brace-of-block-go-down-an-extra-newline-in-golang
;; https://github.com/Fuco1/smartparens/wiki/Permissions#pre-and-post-action-hooks
(sp-pair "{" nil :post-handlers '(("||\n[i]" "RET")))

;; Remove ) auto pair in sh-mode
(with-eval-after-load 'sh-mode
  (sp-local-pair 'sh-mode "(" nil :actions nil))

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

;;;###autoload
(defun my/close-all-parentheses ()
  "Close all parentheses. When smartparens or auto-pair-like functions disabled,
it is convenient to close all parentheses using this function.

Ref: https://acidwords.com/posts/2017-10-19-closing-all-parentheses-at-once.html"
  (interactive "*")
  (let ((closing nil))
    (save-excursion
      (while (condition-case nil
                 (progn
                   (backward-up-list)
                   (let ((syntax (syntax-after (point))))
                     (cl-case (car syntax)
                       ((4) (setq closing (cons (cdr syntax) closing)))
                       ((7 8) (setq closing (cons (char-after (point)) closing)))))
                   t)
               ((scan-error) nil))))
    (apply #'insert (nreverse closing))))

(provide 'init-smartparens)
