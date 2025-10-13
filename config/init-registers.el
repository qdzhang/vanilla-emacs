;;; init-registers.el --- Configure built-in registers  -*- lexical-binding: t; -*-

;;; Commentary:

;; Binding registers key `C-x r' to a transient menu

;;; Code:

(setq register-separator ?+)
(set-register register-separator "\n\n")
(setq register-preview-delay 0.1)

(defun my/bookmarks-list ()
  (interactive)
  (call-interactively 'bookmark-bmenu-list))

(transient-define-prefix my-transient/rectangle-edit-menu ()
  "Rectangle editing menu"
  [
   :description
   (lambda ()
     (propertize "Rectangle edit" 'face 'warning))
   ["Kill"
    ("k" "kill" kill-rectangle)
    ("d" "delete-rectangle" delete-rectangle)
    ("y" "yank-rectangle" yank-rectangle)
    ("c" "clear with space" clear-rectangle)]
   ["Replace"
    ("t" "replace" string-rectangle)
    ("o" "insert space" open-rectangle)
    ("N" "insert number" rectangle-number-lines)]
   ["Copy"
    ("r" "copy rectangle to register" copy-rectangle-to-register)
    ("M-w" "copy-rectangle" copy-rectangle-as-kill)]]
  [
   :description
   (lambda ()
     (propertize "Registers" 'face 'warning))
   ["Text"
    ("+" "increment register" increment-register)
    ("s" "copy to register" copy-to-register)
    ("i" "insert register" insert-register)
    ("v" "view register" view-register)
    ("n" "number to register" number-to-register)]
   ["Point"
    ("j" "jump to register" jump-to-register)
    ("SPC" "point to register" point-to-register)]
   ["Bookmark"
    ("b" "bookmark jump" bookmark-jump)
    ("m" "bookmark set" bookmark-set)
    ("l" "bookmark list" my/bookmarks-list)]
   ["Window"
    ("f" "Save frame" frameset-to-register)
    ("w" "Save window" window-configuration-to-register)]])

(provide 'init-registers)
;;; init-registers.el ends here
