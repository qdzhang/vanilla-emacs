;;; init-lazy-keys.el --- Init for lazy load keys  -*- lexical-binding: t; -*-

;; Make ESC quit prompts
;; (global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Use `comment-line' to replace `comment-dwim'
(global-set-key [remap comment-dwim] #'comment-line)

(lazy-load-global-keys
 '(("C-c g" . my-transient/magit-menu))
 "init-git")

(lazy-load-global-keys
 '(("C-M-s" . my-transient/smartparens-menu))
 "init-smartparens")

(lazy-load-local-keys
 '(("C-M-f" . sp-forward-sexp)
   ("C-M-b" . sp-backward-sexp)
   ("C-M-d" . sp-down-sexp)        ; opposite of `sp-backward-up-sexp'
   ("C-M-u" . sp-backward-up-sexp) ; opposite of `sp-down-sexp'
   ("C-M-k" . sp-kill-sexp)
   ("M-l" . sp-end-of-sexp)
   ("M-u" . sp-up-sexp)            ; opposite of `sp-backward-down-sexp'
   ("M-B" . sp-backward-down-sexp) ; opposite of `sp-up-sexp'
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
   ("M-i" . sp-change-enclosing)
   ("M-o" . sp-rewrap-sexp)
   ("M-d" . my/sp-kill-word)
   ("C-w" . my/kill-region-or-line)
   ("C-;" . sp-comment))
 smartparens-mode-map
 "init-smartparens")


(eval-after-load 'dired
  (lambda ()
    (lazy-load-local-keys
     '(("C-<return>" . my/dired-start-process)
       ("/" . my/dired-filter)
       ("?" . my-transient/dired-help-menu))
     dired-mode-map
     "init-dired")))

(lazy-load-global-keys
 '(("C-c SPC" . my-transient/global-menu))
 "init-transient")

(lazy-load-global-keys
 '(("C-c n" . my/tmp-buffer))
 "init-transient")

(eval-after-load 'org
  (lambda ()
    (lazy-load-local-keys
     '(("C-c p" . my-transient/org-mode-menu)
       ("C-c l" . org-store-link))
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

(lazy-load-global-keys
 '(("C-f" . vimlike-f)
   ("C-b" . vimlike-f-backward))
 "init-navigate")

(lazy-load-unset-keys '("C-x f"))

(lazy-load-set-keys
 '(("C-x f" . my/find-file-fd)))

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
                               '(("C-r" . my/eshell-history)
                                 ("C-l" . my/eshell-clear-keystroke)
                                 ("C-w" . my/kill-word-backward))
                               eshell-mode-map
                               "init-eshell")))

(lazy-load-global-keys
 '(("C-c C-r" . my/sudo-edit)
   ("C-x K" . my/delete-file-and-buffer)
   ("C-x R" . my/rename-file-and-buffer))
 "init-edit")

(lazy-load-global-keys
 '(("M-Q" . my/unfill-paragraph-or-region))
 "init-edit")

(lazy-load-set-keys
 '(("M-/" . hippie-expand)))

(lazy-load-global-keys
 '(("C-<f8>" . my/upcase-word-toggle)
   ("M-c" . my/capitalize-word-toggle)
   ("C-M-<return>" . my/open-newline-below)
   ("C-o" . my/open-newline-above))
 "init-edit")

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
 '(("C-<f11>" . my/ansi-term-toggle)
   ("M-<f11>" . my/ansi-term-split-toggle))
 "init-term")

(eval-after-load 'term
  (lambda ()
    (lazy-load-local-keys
     '(("C-c C-y" . term-paste))
     term-raw-map
     "term")))

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

;; Add a procedure to refresh imenu list manually.
;; https://www.emacswiki.org/emacs/ImenuMode#h5o-4
(lazy-load-set-keys
 '(("M-g I" . (lambda ()
                (interactive)
                (imenu--menubar-select imenu--rescan-item)
                (message "Imenu rescan done.")))))

(lazy-load-global-keys
 '(("r" . my-transient/rectangle-edit-menu))
 "init-registers"
 "C-x")

(lazy-load-global-keys
 '(("C-c b" . my-transient/eww-menu))
 "init-eww")


(lazy-load-global-keys
 '(("C-c d" . inf-sdcv-search))
 "inf-sdcv-mode")

(lazy-load-set-keys
 '(("C-c C-f" . ff-find-other-file)))

(lazy-load-global-keys
 '(("C-x C-5" . my/toggle-frame-split)
   ("C-x v" . shrink-window)
   ("S-<left>" . windmove-left)
   ("S-<right>" . windmove-right)
   ("S-<up>" . windmove-up)
   ("S-<down>" . windmove-down))
 "init-window")

(lazy-load-global-keys
 '(("C-x 8 i" . quick-insert-unicode))
 "quick-insert-unicode")

(with-eval-after-load 'racket-mode
  (lazy-load-local-keys
   '(("C-c p" . my-transient/racket-mode-menu))
   racket-mode-map
   "init-racket"))

(lazy-load-global-keys
 '(("<f6>" . my/toggle-company-mode))
 "init-company")

(lazy-load-set-keys
 '(("M-<up>" . switch-to-prev-buffer)
   ("M-<down>" . switch-to-next-buffer)))

(lazy-load-global-keys
 '(("M-w" . my/kill-ring-save))
 "init-edit")

(provide 'init-lazy-keys)
