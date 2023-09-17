;;; init-perl.el --- Configure perl                  -*- lexical-binding: t; -*-

;;; Commentary:

;; 

;;; Code:

;; indentation
(setq cperl-indent-level 4
      cperl-continued-statement-offset 4
      cperl-indent-parens-as-block t
      cperl-close-paren-offset -4)

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
