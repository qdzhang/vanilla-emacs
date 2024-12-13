;;; init-parens.el -*- lexical-binding: t; -*-

(setq show-paren-style 'paren
      show-paren-delay 0.03
      show-paren-highlight-openparen t
      show-paren-when-point-inside-paren nil
      show-paren-context-when-offscreen 'child-frame
      show-paren-when-point-in-periphery t)

(show-paren-mode 1)

(provide 'init-parens)
