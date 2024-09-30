;;; init-transient.el --- Configure transient        -*- lexical-binding: t; -*-

;;; Commentary:

;; 

;;; Code:

(transient-define-prefix my-transient/common-modes-menu ()
  "A transient menu contains common modes"
  [["Mode"
    ("g" "Glasses mode" glasses-mode)
    ("l" "Lisp mode" lisp-interaction-mode)
    ("s" "Hideshow" hs-minor-mode)
    ("o" "Outline" outline-minor-mode)
    ("p" "Smartparens" smartparens-strict-mode)
    ("E" "Flyspell" flyspell-mode)
    ("e" "Flyspell Prog" flyspell-prog-mode)
    ("m" "Flymake" flymake-mode)]])

(transient-define-prefix my-transient/writing ()
  "A transient for writing."
  [["Wring Utilities"
    ("w" "Writing mode" my/writing-mode)
    ("c" "Count words" my/count-words)
    ("t" "Time now" my/current-time-notify)]])

(defun my/count-words ()
  (interactive)
  (call-interactively 'count-words))

(defun my/current-time-notify ()
  (interactive)
  (notify-os (format-time-string "⏰ %R" (current-time)) "现在时间"))

(transient-define-prefix my-transient/global-menu ()
  "A global transient menu"
  [["Actions"
    ("." "Repeat" repeat :transient t)
    ("d" "Delete whitespace" delete-trailing-whitespace)
    ("r" "Regexp builder" regexp-builder)
    ("b" "Benchmark" benchmark-init/show-durations-tree)
    ("t" "Change theme" my/switch-theme)
    ("i" "Auto insert" auto-insert)]
   ["Proxy"
    ("xs" "Status" proxy-socks-show)
    ("xe" "Enable" proxy-socks-enable)
    ("xd" "Disable" proxy-socks-disable)
    ("xt" "Toggle" proxy-socks-toggle)]
   ["Modes"
    ("m" "Common modes" my-transient/common-modes-menu)
    ("e" "ielm" ielm)
    ("l" "List timers" list-timers)
    ("o" "Tomato timer" tomato-timer)
    ("w" "Writing mode" my-transient/writing)]
   ["Help"
    ("h" "Shortdoc" shortdoc-display-group)
    ("f" "Describe face" describe-face)
    ("k" "Describe keymap" describe-keymap)
    ("q" "Quit" keyboard-quit)]])


;; Create different temp buffers with different major mode, like *scratch*.
;; Ref: https://old.reddit.com/r/emacs/comments/90xkzt/what_do_you_use_the_scratch_buffer_for/e2ulfh5/
;; Why these code are in this file (init-transient.el)?
;; Maybe it will be rewritten by transient :)
(defcustom my/tmp-buffer-mode-alist
  '((?o . org-mode)
    (?t . text-mode)
    (?m . markdown-mode)
    (?s . shell-mode)
    (?l . lisp-interaction-mode))
  "List of major modes for temporary buffers and their hotkeys."
  :type '(alist :key-type character :value-type symbol))

(defun my/tmp-buffer (mode)
  "Open temporary buffer in specified major mode."
  (interactive "c")
  (if (eq mode ?\C-h)
      (with-output-to-temp-buffer "*Help*"
        (princ "Temporary buffers:\n\nKey\tMode\n")
        (dolist (km my/tmp-buffer-mode-alist)
          (princ (format " %c\t%s\n" (car km) (cdr km)))))
    (let ((buf (generate-new-buffer "*tmp*")))
      (with-current-buffer buf
        (funcall (cdr (assoc mode my/tmp-buffer-mode-alist))))
      (pop-to-buffer buf))))

(provide 'init-transient)
;;; init-transient.el ends here
