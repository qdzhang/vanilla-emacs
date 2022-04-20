;;; init-rime.el --- rime config                     -*- lexical-binding: t; -*-

(setq default-input-method "rime")

(with-eval-after-load 'rime
  (setq rime-disable-predicates
        '(rime-predicate-space-after-cc-p
          rime-predicate-org-in-src-block-p
          rime-predicate-prog-in-code-p))

  ;; Change cursor color when input method is opening
  ;; Adapted from https://emacs-china.org/t/topic/17717
  (defvar input-method-cursor-color "Orange"
    "Default cursor color if using an input method.")

  (defun get-frame-cursor-color ()
    "Get the cursor-color of current frame."
    (interactive)
    (frame-parameter nil 'cursor-color))

  (defvar default-cursor-color (get-frame-cursor-color)
    "Default text cursor color.")

  (defun my/rime-disable-p ()
    "Determine whether rime is disabled, following `rime-disable-predicates'"
    (and (rime--should-enable-p)
         (not (rime--should-inline-ascii-p))))

  (defun change-cursor-color-on-input-method ()
    "Set cursor color depending on whether an input method is used or not."
    (interactive)
    (set-cursor-color (if (and (my/rime-disable-p)
                               current-input-method)
                          input-method-cursor-color
                        default-cursor-color)))

  (add-hook 'post-command-hook 'change-cursor-color-on-input-method)

  (setq mode-line-mule-info '((:eval (rime-lighter)))))

(provide 'init-rime)
