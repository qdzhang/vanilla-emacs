;;; init-rime.el --- rime config                     -*- lexical-binding: t; -*-

(setq default-input-method "rime")

(defun rime-predicate-god-mode-p ()
  (and (fboundp 'god-mode)
       (bound-and-true-p god-local-mode)))

(with-eval-after-load 'rime
  (setq rime-disable-predicates
        '(rime-predicate-after-alphabet-char-p
          rime-predicate-space-after-cc-p
          rime-predicate-org-in-src-block-p
          rime-predicate-prog-in-code-p
          rime-predicate-god-mode-p))
  (setq rime-show-candidate 'posframe)
  (setq rime-posframe-style 'vertical)

  ;; 在英文断言成立的时候，按 `M-j' 强制使用中文
  (define-key rime-mode-map (kbd "M-j") 'rime-force-enable)

  ;; 打开 rime 菜单
  (define-key rime-mode-map (kbd "C-`") 'rime-send-keybinding)


  ;; Disable input-method in `isearch-mode'
  ;; If want to use input-method when using isearch
  ;; press `M-e' to edit pattern in minibuffer
  (add-hook 'isearch-mode-hook #'deactivate-input-method)

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

  (add-hook 'post-command-hook 'change-cursor-color-on-input-method))

(provide 'init-rime)
