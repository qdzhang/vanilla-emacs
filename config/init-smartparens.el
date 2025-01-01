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
    ("u" "Up" sp-up-sexp)
    ("U" "Backward Up" sp-backward-up-sexp)
    ("D" "Backward Down" sp-backward-down-sexp)
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

;; Autopair quotes more conservatively; if I'm next to a word/before another
;; quote, I don't want to open a new pair or it would unbalance them.
;; https://github.com/doomemacs/doomemacs/blob/e0385052a8004ec54a402c27357b1352840eb798/modules/config/default/config.el
(let ((unless-list '(sp-point-before-word-p
                     sp-point-after-word-p
                     sp-point-before-same-p)))
  ;; Python f-string will need balanced parens after letter f
  ;; (sp-pair "\"" nil :unless unless-list)
  (sp-pair "'"  nil :unless unless-list))

;; After curly brace, enter `RET' will create an extra newline
;; https://emacs.stackexchange.com/questions/12368/make-ending-curly-brace-of-block-go-down-an-extra-newline-in-golang
;; https://github.com/Fuco1/smartparens/wiki/Permissions#pre-and-post-action-hooks
;;
;; Expand {|} => { | }
;; Expand {|} => {
;;   |
;; }
(dolist (brace '("(" "{" "["))
  (sp-pair brace nil
           :post-handlers '(("||\n[i]" "RET") ("| " "SPC"))
           ;; Don't autopair opening braces if before a word character or
           ;; other opening brace. The rationale: it interferes with manual
           ;; balancing of braces, and is odd form to have s-exps with no
           ;; whitespace in between, e.g. ()()(). Insert whitespace if
           ;; genuinely want to start a new form in the middle of a word.
           :unless '(sp-point-before-word-p sp-point-before-same-p)))

(with-eval-after-load 'js-mode
  (sp-local-pair 'js-mode "<" nil :actions :rem))


;; Remove ) auto pair in sh-mode
(with-eval-after-load 'sh-mode
  (sp-local-pair 'sh-mode "(" nil :actions nil))

;; Remove ` auto pair in `raku-mode'
(with-eval-after-load 'raku-mode
  (sp-local-pair 'raku-mode "`" nil :actions nil))

;; Fix the blank after the | in `ruby-mode'
(with-eval-after-load 'ruby-mode
  (sp-local-pair 'ruby-mode "|" "|" :pre-handlers nil))

;; css comments
(with-eval-after-load 'css-mode
  (sp-local-pair 'css-mode "/* " " */"))


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

;;;###autoload
(defun spacemacs//deactivate-smartparens(&optional global)
  "Deactivate `smartparens-mode'.
This deactivates `smartparens-mode' and `smartparens-strict-mode'.
It is important to disable both to remove all advices.
If `global' is non-nil activate the respective global mode."
  (if global
      (progn
        (when smartparens-global-strict-mode
          (smartparens-global-strict-mode -1))
        (smartparens-global-mode -1))
    (progn
      (when smartparens-strict-mode
        (smartparens-strict-mode -1))
      (smartparens-mode -1))))

;;;###autoload
(defun my/kill-region-or-line (&optional arg)
  "Kill active region or current line. A alternative function to
`sp-kill-region'."
  (interactive "p")
  (if (use-region-p)
      (sp-kill-region (region-beginning) (region-end))
    (sp-kill-whole-line)))

(provide 'init-smartparens)
