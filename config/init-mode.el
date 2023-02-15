;;; init-mode.el --- Config some autoload modes      -*- lexical-binding: t; -*-

;; hook into `hack-local-variables' in order to allow switching spacemacs
;; configurations based on local variables
;; Copied from spacemacs
(add-hook 'hack-local-variables-hook #'spacemacs//run-local-vars-mode-hook)

(defun spacemacs//run-local-vars-mode-hook ()
  "Run a hook for the major-mode after the local variables have been processed."
  (run-hooks (intern (format "%S-local-vars-hook" major-mode))))

;; Autoload major modes
(add-to-list 'auto-mode-alist '("\\.m?jsx?\\'" . js-mode))
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))

;; use cperl-mode instead of perl-mode
(setq auto-mode-alist (rassq-delete-all 'perl-mode auto-mode-alist))
(add-to-list 'auto-mode-alist '("\\.\\([pP]\\([Llm]\\|erl\\|od\\)\\|al\\)\\'" . cperl-mode))

(setq interpreter-mode-alist (rassq-delete-all 'perl-mode interpreter-mode-alist))
(add-to-list 'interpreter-mode-alist '("\\(mini\\)?perl5?" . cperl-mode))

(autoload 'forth-mode "gforth.el")
(autoload 'forth-block-mode "gforth.el")
(add-to-list 'auto-mode-alist '("\\.\\(f\\|fs\\|fth\\|4th\\|frt\\)\\'" . forth-mode))
(add-to-list 'auto-mode-alist '("\\.fb\\'" . forth-block-mode))

(setq arc-program-name "arc -n")
(defvar arc-program-dir (expand-file-name "~/.local/share/anarki/")
  "The directory of Arc with some other tools")
(autoload 'arc-mode (concat arc-program-dir "extras/arc.el"))
(add-to-list 'auto-mode-alist '("\\.arc\\'" . arc-mode))
(add-hook 'arc-mode-hook (lambda ()
                           (load-file (concat arc-program-dir "extras/inferior-arc.el"))
                           (require 'inferior-arc)))

(add-to-list 'auto-mode-alist '("/Eask\\'" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("/Easkfile\\'" . emacs-lisp-mode))

(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))

;; Mode load
(autoload 'meson-mode "meson-mode")
(autoload 'janet-mode "janet-mode")
(autoload 'markdown-mode "init-markdown")
(autoload 'gfm-mode "init-markdown")
(autoload 'ruby-mode "init-ruby")
(autoload 'inf-ruby-minor-mode "init-ruby")
;; (autoload 'erb-mode "erb-mode")

(autoload 'racket-xp-mode "init-racket")
(autoload 'racket-mode "init-racket")

;; Config `Info-mode'
(autoload 'my/info-mode-font-setup "init-info")
(add-hook 'Info-mode-hook #'my/info-mode-font-setup)

(provide 'init-mode)
