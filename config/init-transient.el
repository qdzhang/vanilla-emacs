;;; init-transient.el --- Configure transient        -*- lexical-binding: t; -*-

;;; Commentary:

;; 

;;; Code:


(transient-define-prefix my-transient/global-menu ()
  "A global transient menu"
  [["Move"
    ("n" "Next line" next-line :transient t)
    ("p" "Previous line" previous-line :transient t)
    ("f" "Forward char" forward-char :transient t)
    ("b" "Backward char" backward-char :transient t)
    ("." "Repeat" repeat :transient t)]
   ["Help"
    ("h" "Shortdoc" shortdoc-display-group)]])


(provide 'init-transient)
;;; init-transient.el ends here
