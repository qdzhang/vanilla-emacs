;;; init-c.el --- cc-mode and semantic config        -*- lexical-binding: t; -*-

;; There are three approaches for editing and navigating in c projects.
;; Config the variable `my/c-mode-selection' to select which one to use.
(defvar my/c-mode-selection 'clang
  "This variable determines which part of configurations will be used in c-mode.
Available values are:
  - clang: use `company-clang' to auto complete and gtags to navigate.
  - cedet: use semantic-relative libraries.
  - eglot: use lsp.")

(setq my/c-mode-selection 'clang)

(cl-case my/c-mode-selection
  (clang (add-hook 'c-mode-hook 'my/c-mode-to-use-clang-and-gtags))
  (cedet (add-hook 'c-mode-hook 'my/c-mode-to-use-semantic))
  (eglot (add-hook 'c-mode-hook 'my/c-mode-to-use-eglot)))


;; Setup CEDET
;;
;; A Gentle introduction to CEDET
;; https://alexott.net/en/writings/emacs-devenv/EmacsCedet.html
;;
(defun my/c-mode-to-use-semantic ()
  (semantic-mode 1)

  ;; system header files
  (require 'semantic/bovine/gcc)
  (require 'semantic/bovine/c)
  ;; name completion
  (require 'semantic/ia)

  (require 'semantic/util)

  (local-set-key "\C-c\C-j" 'semantic-ia-fast-jump)
  (local-set-key "\C-c\C-s" 'semantic-ia-show-summary)
  (local-set-key "\C-c\C-h" 'semantic-ia-show-doc)
  (local-set-key "\C-c?" 'semantic-ia-complete-symbol-menu)
  (local-set-key "\C-ct" 'semantic-analyze-proto-impl-toggle)
  (local-set-key (kbd "C-c , d") 'semantic-ia-describe-class)
  (local-set-key (kbd "C-c , -") 'senator-fold-tag)
  (local-set-key (kbd "C-c , +") 'senator-unfold-tag)
  (local-set-key (kbd "C-c , f") 'senator-search-set-tag-class-filter)
  (local-set-key (kbd "C-c , s") 'senator-search-forward)
  (local-set-key (kbd "C-c , S") 'senator-search-backward))


(defun my/semantic-hook ()
  "The hook used when semantic initialize"
  (imenu-add-to-menubar "TAGS"))

(add-hook 'semantic-init-hook 'my/semantic-hook)

;; Disable Semantic in all non-cc-mode buffers.
;; https://stackoverflow.com/a/14094946
(setq semantic-inhibit-functions
      (list (lambda () (not (and (featurep 'cc-defs)
                                 c-buffer-is-cc-mode)))))

(with-eval-after-load 'semantic
  (advice-add 'semantic-idle-scheduler-function :around #'ignore)

  ;; Make semantic use built-in project.el
  (setq semanticdb-project-root-functions #'my/semantic-project-root)
  ;; (semantic-add-system-include "/usr/include/gtk-3.0/" 'c-mode)


  (add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode))


;; Setup gtags
(defun my/c-mode-to-use-clang-and-gtags ()
  "Use `company-clang' to auto-complete and `gtags-mode' to navigate in projects"
  ;; Load tags config
  (require 'init-tags)

  (gtags-mode 1)

  ;; There is another [gtags-mode](https://github.com/Ergus/gtags-mode). It is
  ;; a good alternative gtags interface to the built-in one. This mode provides
  ;; great integration with the built-in facilities, including xref, project,
  ;; completion-at-point (capf) and imenu.
  ;; But this gtags-mode package defines a global minor mode, to inhibit
  ;; `gtags-mode' in other major modes, uncomment the following code fragment.
  ;;
  ;; (add-hook 'buffer-list-update-hook
  ;;           (lambda ()
  ;;             (gtags-mode (if (derived-mode-p 'c-mode) 1 -1))))

  (with-eval-after-load 'company
    (setq-local company-backends '(company-clang company-capf))))


;; Setup eglot
(defun my/c-mode-to-use-eglot ()
  "Load init-eglot.el and enable `eglot'"
  ;; load eglot config
  (require 'init-eglot)
  (eglot-ensure))


(with-eval-after-load 'company
  (add-hook 'meson-mode-hook 'company-mode))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(setq comment-style 'extra-line)


;; Config cc-mode style
;; https://github.com/llvm/llvm-project/blob/main/llvm/utils/emacs/emacs.el
(defun llvm-lineup-statement (langelem)
  (let ((in-assign (c-lineup-assignments langelem)))
    (if (not in-assign)
        '++
      (aset in-assign 0
            (+ (aref in-assign 0)
               (* 2 c-basic-offset)))
      in-assign)))

;; Add llvm.org style
(c-add-style "llvm.org"
             '("gnu"
               (fill-column . 80)
               (c++-indent-level . 2)
               (c-basic-offset . 2)
               (indent-tabs-mode . nil)
               (c-offsets-alist . ((arglist-intro . ++)
                                   (innamespace . 0)
                                   (member-init-intro . ++)
                                   (statement-cont . llvm-lineup-statement)))))

(add-hook 'c-mode-common-hook
          (function
           (lambda nil
             (c-set-style "llvm.org"))))

;; Enable `smart-semicolon-mode' in `c-mode'
(add-hook 'c-mode-common-hook #'smart-semicolon-mode)

(defun my/meson-executable ()
  "Insert executable function of current file in meson.build."
  (interactive)
  (let* ((file-name-ext (file-name-nondirectory (buffer-file-name)))
         (file-name (file-name-base file-name-ext)))
    (find-file "meson.build")
    (goto-char (point-max))
    (insert (concat
             "executable('" file-name "', '" file-name-ext "')"))))

(defun my/c-abbrev-config ()
  "Configurations of abbrev in `c-mode'"
  (abbrev-mode 1)
  (define-abbrev c-mode-abbrev-table "icl"
    "" 'my-skel/include-system)
  (define-abbrev c-mode-abbrev-table "iclu"
    "" 'my-skel/include-user))

(defun my/c-flymake ()
  "Configurations of flymake in `c-mode'"
  (require 'init-flymake)
  (add-hook 'flymake-diagnostic-functions 'flymake-gcc nil t))

(add-hook 'c-mode-hook (lambda ()
                         (smart-dash-mode 1)
                         (my/c-abbrev-config)
                         (my/c-flymake)))

(require 'editcmacro)
(add-hook 'c-mode-hook   #'editcmacro-mode)
(add-hook 'c++-mode-hook #'editcmacro-mode)

;; https://github.com/Elilif/.elemacs/blob/ff4f2e3076de5aa653479f37b77d294940d0a828/lisp/init-lang.el#L95C1-L108C74
(defun my/compile-set ()
  "Setup `compile' for `c-mode' and `c++-mode'."
  (let* ((file-name (buffer-file-name))
         (is-windows (equal 'windows-nt system-type))
         (exec-suffix (if is-windows ".exe" ".out")))
    (when file-name
      (setq file-name (file-name-nondirectory file-name))
      (let ((out-file (concat (file-name-sans-extension file-name) exec-suffix)))
        (setq-local compile-command (pcase major-mode
                                      ('c++-mode
                                       (format "g++ -std=c++11 -g %s -o %s"
                                               file-name out-file))
                                      ('c-mode
                                       (format "gcc -std=c89 -g %s -o %s"
                                               file-name out-file))))))))

(add-hook 'c-mode-common-hook 'my/compile-set)

(provide 'init-c)
