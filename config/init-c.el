;;; init-c.el --- cc-mode and semantic config        -*- lexical-binding: t; -*-


(defun my/c-mode-to-use-semantic ()
  (semantic-mode 1)

  ;; system header files
  (require 'semantic/bovine/gcc)
  ;; name completion
  (require 'semantic/ia)

  (local-set-key "\C-c\C-j" 'semantic-ia-fast-jump)
  (local-set-key "\C-c\C-s" 'semantic-ia-show-summary)
  (local-set-key "\C-c\C-h" 'semantic-ia-show-doc)
  (local-set-key "\C-c?" 'semantic-ia-complete-symbol-menu)
  (local-set-key "\C-ct" 'semantic-analyze-proto-impl-toggle)
  (local-set-key (kbd "C-c , -") 'senator-fold-tag)
  (local-set-key (kbd "C-c , +") 'senator-unfold-tag)
  (local-set-key (kbd "C-c , f") 'senator-search-set-tag-class-filter)
  (local-set-key (kbd "C-c , s") 'senator-search-forward)
  (local-set-key (kbd "C-c , S") 'senator-search-backward))


(add-hook 'c-mode-hook 'my/c-mode-to-use-semantic)
(add-hook 'c++-mode-hook ' my/c-mode-to-use-semantic)

(defun my/semantic-hook ()
  "The hook used when semantic initialize"
  (imenu-add-to-menubar "TAGS"))

(add-hook 'semantic-init-hook 'my/semantic-hook)

(with-eval-after-load 'semantic
  (advice-add 'semantic-idle-scheduler-function :around #'ignore)

  (global-semanticdb-minor-mode 1)
  (global-semantic-idle-scheduler-mode 1)
  (global-semantic-highlight-edits-mode 1)
  (global-semantic-mru-bookmark-mode 1)

  ;; Make semantic use built-in project.el
  (setq semanticdb-project-root-functions #'my/semantic-project-root)
  ;; (semantic-add-system-include "/usr/include/gtk-3.0/" 'c-mode)
  )


(with-eval-after-load 'company
  (add-hook 'meson-mode-hook 'company-mode))

(c-add-style "csapp"
             '("gnu"
               (c-basic-offset . 4)     ; Guessed value
               (c-offsets-alist
                (defun-block-intro . +) ; Guessed value
                (defun-close . 0)       ; Guessed value
                (statement . 0)         ; Guessed value
                (topmost-intro . 0)     ; Guessed value
                (access-label . -)
                (annotation-top-cont . 0)
                (annotation-var-cont . +)
                (arglist-close . c-lineup-close-paren)
                (arglist-cont c-lineup-gcc-asm-reg 0)
                (arglist-cont-nonempty . c-lineup-arglist)
                (arglist-intro . c-lineup-arglist-intro-after-paren)
                (block-close . 0)
                (block-open . 0)
                (brace-entry-open . 0)
                (brace-list-close . 0)
                (brace-list-entry . 0)
                (brace-list-intro first c-lineup-2nd-brace-entry-in-arglist c-lineup-class-decl-init-+ +)
                (brace-list-open . +)
                (c . c-lineup-C-comments)
                (case-label . 0)
                (catch-clause . 0)
                (class-close . 0)
                (class-open . 0)
                (comment-intro . c-lineup-comment)
                (composition-close . 0)
                (composition-open . 0)
                (cpp-define-intro c-lineup-cpp-define +)
                (cpp-macro . -1000)
                (cpp-macro-cont . +)
                (defun-open . 0)
                (do-while-closure . 0)
                (else-clause . 0)
                (extern-lang-close . 0)
                (extern-lang-open . 0)
                (friend . 0)
                (func-decl-cont . +)
                (inclass . +)
                (incomposition . +)
                (inexpr-class . +)
                (inexpr-statement . +)
                (inextern-lang . +)
                (inher-cont . c-lineup-multi-inher)
                (inher-intro . +)
                (inlambda . 0)
                (inline-close . 0)
                (inline-open . 0)
                (inmodule . +)
                (innamespace . +)
                (knr-argdecl . 0)
                (knr-argdecl-intro . 5)
                (label . 0)
                (lambda-intro-cont . +)
                (member-init-cont . c-lineup-multi-inher)
                (member-init-intro . +)
                (module-close . 0)
                (module-open . 0)
                (namespace-close . 0)
                (namespace-open . 0)
                (objc-method-args-cont . c-lineup-ObjC-method-args)
                (objc-method-call-cont c-lineup-ObjC-method-call-colons c-lineup-ObjC-method-call +)
                (objc-method-intro .
                                   [0])
                (statement-block-intro . +)
                (statement-case-intro . +)
                (statement-case-open . +)
                (statement-cont . +)
                (stream-op . c-lineup-streamop)
                (string . -1000)
                (substatement . +)
                (substatement-label . 0)
                (substatement-open . +)
                (template-args-cont c-lineup-template-args +)
                (topmost-intro-cont first c-lineup-topmost-intro-cont c-lineup-gnu-DEFUN-intro-cont))))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq c-default-style "csapp")
(setq c-basic-offset 4)
(setq comment-style 'extra-line)

(provide 'init-c)
