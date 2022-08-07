;;; init-transient.el --- Configure transient        -*- lexical-binding: t; -*-

;;; Commentary:

;; 

;;; Code:


(transient-define-prefix my-transient/global-menu ()
  "A global transient menu"
  [["Actions"
    ("." "Repeat" repeat :transient t)
    ("d" "Delete whitespace" delete-trailing-whitespace)
    ("r" "Regexp builder" regexp-builder)]
   ["Proxy"
    ("xs" "Status" proxy-socks-show)
    ("xe" "Enable" proxy-socks-enable)
    ("xd" "Disable" proxy-socks-disable)
    ("xt" "Toggle" proxy-socks-toggle)]
   ["Mode"
    ("g" "Glasses mode" glasses-mode)
    ("s" "Hideshow" hs-minor-mode)
    ("o" "Outline" outline-minor-mode)
    ("p" "Smartparens" smartparens-strict-mode)]
   ["Help"
    ("h" "Shortdoc" shortdoc-display-group)
    ("f" "Describe face" describe-face)
    ("q" "Quit" keyboard-quit)]])


(provide 'init-transient)
;;; init-transient.el ends here
