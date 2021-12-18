;;; init-edit.el -*- lexical-binding: t; -*-

(require 'easy-kill)

(global-set-key [remap kill-ring-save] #'easy-kill)
(global-set-key [remap mark-sexp] #'easy-mark)

(provide 'init-edit)
