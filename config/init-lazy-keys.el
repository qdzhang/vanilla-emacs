;;; init-lazy-keys.el --- Init for lazy load keys

;;; newline-without-break-of-line
;;;###autoload
(defun my/newline-without-break-of-line ()
  "1. move to end of the line.
  2. insert newline with index"

  (interactive)
  (let ((oldpos (point)))
    (end-of-line)
    (newline-and-indent)))

;; (global-set-key (kbd "<C-return>") 'my/newline-without-break-of-line)
;; (lazy-load-global-keys
;;  '(
;;    ("C-<return>" . my/newline-without-break-of-line)))

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Use `comment-line' to replace `comment-dwim'
(global-set-key [remap comment-dwim] #'comment-line)


;;; === Git config ===

(lazy-load-global-keys
 '(
   ("C-c g" . one-key-menu-magit))
 "init-git")

;;; === Git config ===

;; (lazy-load-unset-keys
;;  '("M-J" "M-r" "M-s" "M-;" "C-M-f" "C-M-b" "M-)")
;;  smartparens-mode-map)             ;卸载按键
;; (defvar smartparens-key-alist nil)
;; (setq smartparens-key-alist
;;       '(

;;         ))
;; (lazy-load-set-keys smartparens-key-alist smartparens-mode-map)

(lazy-load-global-keys
 '(
   ("C-M-s" . one-key-menu-smartparens))
 "init-smartparens")

(lazy-load-local-keys
 '(
   ("M-<delete>" . sp-unwrap-sexp)
   ("M-<backspace>" . sp-backward-unwrap-sexp)
   ("C-<right>". sp-forward-slurp-sexp)
   ("C-<left>" . sp-forward-barf-sexp)
   ("C-M-<left>" . sp-backward-slurp-sexp)
   ("C-M-<right>" . sp-backward-barf-sexp)
   ("M-D" . sp-splice-sexp)
   ("C-M-<delete>" . sp-splice-sexp-killing-forward)
   ("C-M-<backspace>" . sp-splice-sexp-killing-backward)
   ("C-]" . sp-select-next-thing-exchange)
   ("C-\"" . sp-change-inner)
   ("M-o" . my/sp-new-line)
   ("M-i" . sp-change-enclosing))
 
 smartparens-mode-map
 "init-smartparens")

(lazy-load-global-keys
 '(
   ("C-c d" . one-key-menu-dired))
 "init-dired")


(provide 'init-lazy-keys)
