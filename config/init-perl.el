;;; init-perl.el --- Configure perl                  -*- lexical-binding: t; -*-

;;; Commentary:

;; Config perl and raku

;;; Code:

;; indentation
(setq cperl-indent-level 4
      cperl-continued-statement-offset 4
      cperl-indent-parens-as-block t
      cperl-close-paren-offset -4)


(defun my/perl-mode-hook ()
  "Custom `cperl-mode' behaviours."
  ;; (my/perl-key-translation)
  (flymake-mode 1)
  (smart-dash-mode 1)
  (add-to-list 'smart-dash-c-modes 'cperl-mode))

;; https://emacs.stackexchange.com/questions/39229/perform-key-translation-in-a-major-mode
(defun my/perl-key-translation ()
  ;; Buffer-local key translation from "4" to "$".
  (let ((keymap (make-sparse-keymap)))
    (set-keymap-parent keymap key-translation-map)
    (setq-local key-translation-map keymap)
    (define-key key-translation-map (kbd "4") (kbd "$"))
    (define-key key-translation-map (kbd "$") (kbd "4"))))

(add-hook 'cperl-mode-hook #'my/perl-mode-hook)

(with-eval-after-load 'cperl-mode
  (define-key cperl-mode-map (kbd "C-M-<tab>") 'perltidy-region)
  (define-key cperl-mode-map (kbd "C-c <f1>") 'perltidy-buffer))


;; Raku
(setq raku-exec-path (executable-find "raku"))

;; raku tamplate skeletons
(define-auto-insert
  '("\\.rakumod\\'" . "Raku module skeleton")
  'raku-module-skeleton)

(define-auto-insert
  '("\\.raku\\'" . "Raku script skeleton")
  'raku-script-skeleton)


(provide 'init-perl)
;;; init-perl.el ends here
