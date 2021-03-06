;;; init-lazy-keys.el --- Init for lazy load keys  -*- lexical-binding: t; -*-

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
;; (global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Use `comment-line' to replace `comment-dwim'
(global-set-key [remap comment-dwim] #'comment-line)


;;; === Git config ===

(lazy-load-global-keys
 '(("C-c g" . my-transient/magit-menu))
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
 '(("C-M-s" . my-transient/smartparens-menu))
 "init-smartparens")

(lazy-load-local-keys
 '(("C-M-f" . sp-forward-sexp)
   ("C-M-b" . sp-backward-sexp)
   ("C-M-d" . sp-down-sexp)
   ("C-M-u" . sp-backward-up-sexp)
   ("C-M-k" . sp-kill-sexp)
   ("M-l" . sp-end-of-sexp)
   ("M-u" . sp-up-sexp)
   ("M-[" . sp-backward-unwrap-sexp)
   ("M-]" . sp-unwrap-sexp)
   ("C-<right>". sp-forward-slurp-sexp)
   ("C-<left>" . sp-forward-barf-sexp)
   ("C-M-<left>" . sp-backward-slurp-sexp)
   ("C-M-<right>" . sp-backward-barf-sexp)
   ("M-D" . sp-splice-sexp)
   ("C-M-<delete>" . sp-splice-sexp-killing-forward)
   ("C-M-<backspace>" . sp-splice-sexp-killing-backward)
   ("C-]" . sp-select-next-thing-exchange)
   ("C-\"" . sp-change-inner)
   ("M-r" . sp-delete-region)
   ("M-o" . my/sp-new-line)
   ("M-i" . sp-change-enclosing))

 smartparens-mode-map
 "init-smartparens")


(eval-after-load 'dired
  (lambda ()
    (lazy-load-local-keys
     '(("C-c d" . my-transient/dired-menu)
       ("C-<return>" . my/dired-start-process)
       ("/" . my/dired-filter))
     dired-mode-map
     "init-dired")))

(lazy-load-global-keys
 '(("C-c ," . my-transient/global-menu))
 "init-transient")

(eval-after-load 'org
  (lambda ()
    (lazy-load-local-keys
     '(("C-c ," . my-transient/org-mode-menu))
     org-mode-map
     "init-org")))

(lazy-load-global-keys
 '(("C-c s" . rg-menu))
 "init-grep")

(lazy-load-local-keys
 '(("M-s r" . rg-isearch-menu))
 isearch-mode-map
 "init-grep")

(lazy-load-local-keys
 '(("M-s t" . my/toggle-search-whitespace))
 isearch-mode-map
 "init-isearch")

(lazy-load-unset-keys '("C-x f"))

(lazy-load-set-keys
 '(("C-x f" . find-lisp-find-dired)))

(lazy-load-global-keys
 '(("C-x p" . my-transient/project-menu))
 "init-project")

(lazy-load-global-keys
 '(("C-x t" . my-transient/tab-bar-menu)
   ("M-g t" . tab-bar-switch-to-tab))
 "init-tab-bar")


;; According to thie issues, eshell can't bind keys in eshll-mode-map this time
;; Use hook instead
;; https://github.com/noctuid/general.el/issues/80
(add-hook 'eshell-mode-hook (lambda ()
                              (lazy-load-local-keys
                               '(("C-r" . my/eshell-history))
                               eshell-mode-map
                               "init-eshell")))

(lazy-load-global-keys
 '(("C-c C-r" . my/sudo-edit))
 "init-edit")

(lazy-load-global-keys
 '(("M-Q" . my/unfill-paragraph-or-region))
 "init-edit")

(lazy-load-set-keys
 '(("M-/" . hippie-expand)))

(add-hook 'c-initialization-hook
          (lambda ()
            (lazy-load-local-keys
             '(("C-M-<tab>" . clang-format-region)
               ("C-c <f2>" . clang-rename))
             c-mode-base-map
             "init-clang-tools")))

(lazy-load-global-keys
 '(("C-c o e" . eglot)
   ("C-c o E" . eglot-shutdown))
 "init-eglot")

(eval-after-load 'flymake
  (lambda ()
    (lazy-load-local-keys
     '(("C-c ! l" . flymake-show-buffer-diagnostics)
       ("C-c ! n" . flymake-goto-next-error)
       ("C-c ! p" . flymake-goto-prev-error))
     flymake-mode-map
     "init-flymake")))

(lazy-load-global-keys
 '(("C-`" . sane-eshell)
   ("<f12>" . sane-eshell-create)
   ("C-<f12>" . sane-eshell-create-in-project-root))
 "sane-eshell")

(lazy-load-global-keys
 '(("c" . org-capture))
 "org-capture"
 "C-c")

(lazy-load-global-keys
 '(("a" . org-agenda))
 "org-agenda"
 "C-c")

;; In emacs 29, `imenu' is bound to `M-g i' globally
;; https://git.savannah.gnu.org/cgit/emacs.git/diff/etc/NEWS?id=ad89ec84ee20a027e36922c187ad9f2dcb93bcaf&id2=783dd6da31e3f0387e110972c0b9fe1f5acc4bba
(when (< emacs-major-version 29)
  (lazy-load-set-keys
   '(("M-g i" . imenu))))

(provide 'init-lazy-keys)
