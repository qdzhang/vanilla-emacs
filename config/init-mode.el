;;; init-mode.el --- Config some autoload modes      -*- lexical-binding: t; -*-

;; hook into `hack-local-variables' in order to allow switching spacemacs
;; configurations based on local variables
;; Copied from spacemacs
(add-hook 'hack-local-variables-hook #'spacemacs//run-local-vars-mode-hook)

(defun spacemacs//run-local-vars-mode-hook ()
  "Run a hook for the major-mode after the local variables have been processed."
  (run-hooks (intern (format "%S-local-vars-hook" major-mode))))

;; Autoload major modes
;; (add-to-list 'auto-mode-alist '("\\.m?jsx?\\'" . js-mode))
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.[pxr]?html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.njk\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.liquid\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tt$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ep$" . web-mode))

;; use cperl-mode instead of perl-mode
(setq auto-mode-alist (rassq-delete-all 'perl-mode auto-mode-alist))
(add-to-list 'auto-mode-alist '("\\.\\([pP]\\([Llm]\\|erl\\|od\\)\\|al\\)\\'" . cperl-mode))
(add-to-list 'auto-mode-alist '("\\.psgi$" . cperl-mode))
(add-to-list 'auto-mode-alist '("cpanfile$" . cperl-mode))
(add-to-list 'auto-mode-alist '("\\.t$" . cperl-mode))

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

(add-to-list 'auto-mode-alist '("\\.irbrc.*\\'" . ruby-mode))

(define-derived-mode my-erb-mode web-mode "MyErb"
  "A major mode derived from web-mode, for editing .erb files.")
(add-to-list 'auto-mode-alist '("\\.erb\\'" . my-erb-mode))


;; (autoload 'erb-mode "erb-mode")

(autoload 'racket-xp-mode "init-racket")
(autoload 'racket-mode "init-racket")

;; Config `Info-mode'
(autoload 'my/info-mode-font-setup "init-info")
(add-hook 'Info-mode-hook #'my/info-mode-font-setup)

;; Config astro mode
(define-derived-mode astro-mode web-mode "astro")
(setq auto-mode-alist
      (append '((".*\\.astro\\'" . astro-mode))
              auto-mode-alist))

(add-to-list 'auto-mode-alist '("\\.cjs\\'" . javascript-mode))

(add-to-list 'auto-mode-alist '("\\.d2\\'" . d2-mode))

;; Make .env file use `conf-mode'
(add-to-list 'auto-mode-alist '("\\.env.test$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.env.local$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.env.sample$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.env$" . conf-mode))

;; Use and `tsx-ts-mode'
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . tsx-ts-mode))

;; Use `bash-ts-mode'
(add-to-list 'major-mode-remap-alist '(shell-script-mode . bash-ts-mode))
(add-to-list 'major-mode-remap-alist '(sh-mode . bash-ts-mode))
;; Use `json-ts-mode'
(add-to-list 'major-mode-remap-alist '(js-json-mode . json-ts-mode))


(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))

(add-to-list 'auto-mode-alist
             '("\\(?:CMakeLists\\.txt\\|\\.cmake\\)\\'" . cmake-ts-mode))

(add-to-list 'auto-mode-alist
             '("\\.\\(pas\\|lpr\\|dpr\\|pp\\|inc\\|dpk\\)\\'" . opascal-mode))

(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))


(provide 'init-mode)
