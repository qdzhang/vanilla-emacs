;;; init-c.el --- cc-mode and semantic config        -*- lexical-binding: t; -*-

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


(add-hook 'c-mode-hook 'my/c-mode-to-use-semantic)
(add-hook 'c++-mode-hook ' my/c-mode-to-use-semantic)

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

(provide 'init-c)
