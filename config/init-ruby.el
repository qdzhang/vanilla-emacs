;;; init-ruby.el --- Config ruby mode                -*- lexical-binding: t; -*-

;;; Commentary:

;; Config ruby mode

;;; Code:

(require 'inf-ruby)

(autoload 'inf-ruby-minor-mode "inf-ruby" "Run an inferior Ruby process" t)
(add-hook 'ruby-mode-hook (lambda ()
                            (smart-dash-mode 1)
                            (inf-ruby-minor-mode 1)
                            (subword-mode 1)
                            (rbtagger-mode)
                            (stree-format-on-save-mode 1)))


;; Auto-update rbtagger tags when ruby files change
;;
;; (add-hook 'after-save-hook
;;           (lambda ()
;;             (if (eq major-mode 'ruby-mode)
;;                 (call-interactively 'rbtagger-generate-tags))))

(add-hook
 'rbtagger-after-generate-tag-hook
 (lambda (success project-name)
   (if success
       (notify-os (concat project-name " Tags ✅") "Ruby tags successfully generated")
     (notify-os "Is this a Ruby project? Tags FAILED! ⚠" "Failed!!!"))))

(transient-define-prefix my-transient/ruby-mode ()
  "A transient menu for ruby mode"
  [["rbtagger"
    ("v" "Visit tags table" visit-tags-table)
    ("g" "Generate tags" rbtagger-generate-tags)
    ("l" "Show logs" rbtagger-stdout-log)]])

;; Don't auto-insert encoding comments
;; Those are almost never needed in Ruby 2+
(setq ruby-insert-encoding-magic-comment nil)

(setq ruby-deep-indent-paren nil)

(eval-after-load "hideshow"
  '(add-to-list 'hs-special-modes-alist
                `(ruby-mode
                  ,(rx (or "def" "class" "module" "do" "{" "[" "if" "else" "unless"))
                  ,(rx (or "}" "]" "end"))
                  ,(rx (or "#" "=begin"))
                  ruby-forward-sexp nil)))

(add-hook 'ruby-mode-hook #'hs-minor-mode)

;; Use direnv to instead of chruby
(require 'chruby)
(add-hook 'ruby-mode-hook #'chruby-use-corresponding)

(with-eval-after-load 'web-mode
  (setq web-mode-markup-indent-offset 2))

(with-eval-after-load 'ruby-mode
  (defun my/ruby-send-line-and-next-line ()
    (interactive)
    (ruby-send-line)
    (next-line))

  (define-key ruby-mode-map (kbd "C-c C-j") 'my/ruby-send-line-and-next-line)
  (define-key ruby-mode-map (kbd "C-c C-n") 'ruby-send-line))

;; Highlight debugger lines
(defun my/ruby-maybe-highlight-debugger-keywords ()
  "Highlight break point lines."
  (interactive)
  (highlight-lines-matching-regexp "byebug")
  (highlight-lines-matching-regexp "binding.break")
  (highlight-lines-matching-regexp "binding.irb")
  (highlight-lines-matching-regexp "binding.pry"))

(add-hook 'ruby-mode-local-vars-hook 'my/ruby-maybe-highlight-debugger-keywords)


(provide 'init-ruby)
;;; init-ruby.el ends here
