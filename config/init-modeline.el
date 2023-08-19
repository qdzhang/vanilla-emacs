;;; init-modeline.el --- Config modeline             -*- lexical-binding: t; -*-

;;; Commentary:

;; Config modeline and diminish some minor modes

;;; Code:

;; Hide all minor mode lighter
(setq mode-line-modes
      (mapcar (lambda (elem)
                (pcase elem
                  (`(:propertize (,_ minor-mode-alist . ,_) . ,_)
                   "")
                  (_ elem)))
              mode-line-modes))

;; Set up header-line
(which-function-mode)

;; Copy from https://old.reddit.com/r/emacs/comments/36fzct/any_good_configs_of_the_emacs_header_line/crduybv/
(setq which-func-unknown "⊥" ; The default is really boring…
      which-func-format `((:propertize (" ➤ " which-func-current)
                                       local-map ,which-func-keymap
                                       face which-func
                                       mouse-face mode-line-highlight
                                       help-echo "mouse-1: go to beginning\n\
      mouse-2: toggle rest visibility\n\
      mouse-3: go to end")))

(setq-default header-line-format
              `("%4l "
                which-func-mode ("" which-func-format " ")
                " ➤ "
                (:propertize default-directory face mode-line-buffer-id)))



;; Remove which-function-mode from mode-line
(setq mode-line-misc-info
      (delete '(which-function-mode (which-func-mode ("" which-func-format " ")))
              mode-line-misc-info))

;; Hide mode line
;; Ref: https://bzg.fr/en/emacs-hide-mode-line/
(defvar-local hidden-mode-line-mode nil)

(define-minor-mode hidden-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global t
  :variable hidden-mode-line-mode
  :group 'editing-basics
  (if hidden-mode-line-mode
      (setq hide-mode-line mode-line-format
            mode-line-format nil)
    (setq mode-line-format hide-mode-line
          hide-mode-line nil))
  (force-mode-line-update)
  ;; Apparently force-mode-line-update is not always enough to
  ;; redisplay the mode-line
  (redraw-display)
  (when (and (called-interactively-p 'interactive)
             hidden-mode-line-mode)
    (run-with-idle-timer
     0 nil 'message
     (concat "Hidden Mode Line Mode enabled.  "
             "Use M-x hidden-mode-line-mode to make the mode-line appear."))))



(provide 'init-modeline)
;;; init-modeline.el ends here
