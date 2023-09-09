;;; init-navigate.el --- Configure vim-like jumplist  -*- lexical-binding: t; -*-

;;; Commentary:

;; Define some usefult navigation operators.
;;
;; - Use `jumplist' to jump previous or next positions.
;; - Use `repeat-mode' to define some keymaps, to reduce the need to hold down
;;   modifier keys when enter commands
;; - Define two functions to mimic vim `f' key, to look forward(backward) in a
;;   single line, and repeat until the target char;

;;; Code:

(require 'jumplist)
(global-set-key (kbd "C-<") 'jumplist-previous)
(global-set-key (kbd "C->") 'jumplist-next)
(custom-set-variables
 '(jumplist-hook-commands
   '(dired-jump isearch-forward isearch-backward end-of-buffer beginning-of-buffer find-file
                xref-find-definitions xref-find-references rg-literal rg-dwim rg-dwim-project-dir
                rg-dwim-current-dir next-error-no-select previous-error-no-select
                bookmark-jump tab-bar-switch-to-tab tab-bar-new-tab))
 '(jumplist-ex-mode t))

(defvar up-down-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'next-line)
    (define-key map (kbd "p") #'previous-line)
    map)
  "Keymap to repeat `next-line' and `previous-line' with `n' and `p'")

(dolist (cmd '(next-line previous-line))
  (put cmd 'repeat-map 'up-down-repeat-map))


(defvar word-navigation-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "f") #'forward-word)
    (define-key map (kbd "b") #'backward-word)
    map)
  "Keymap to repeat `forward-word' and `backward-word'")

(dolist (cmd '(forward-word backward-word))
  (put cmd 'repeat-map 'word-navigation-repeat-map))

(defvar defun-navigation-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "e") #'end-of-defun)
    (define-key map (kbd "a") #'beginning-of-defun)
    map)
  "Keymap to repeat functions navigation key sequences.")

(dolist (cmd '(end-of-defun beginning-of-defun))
  (put cmd 'repeat-map 'defun-navigation-repeat-map))

(defvar recenter-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "l") #'recenter-top-bottom)
    map)
  "Keymap to repeat recenter function.")

(put 'recenter-top-bottom 'repeat-map 'recenter-repeat-map)

(defun my/scroll-down (&optional lines)
  (interactive)
  (if lines
      (pixel-scroll-precision-interpolate (* -1 lines (pixel-line-height)))
    (pixel-scroll-interpolate-up)))

(defun my/scroll-up (&optional lines)
  (interactive)
  (if lines
      (pixel-scroll-precision-interpolate (* lines (pixel-line-height))))
  (pixel-scroll-interpolate-down))

(provide 'init-navigate)
;;; init-navigate.el ends here
