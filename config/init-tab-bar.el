;;; init-tab-bar.el --- tab-bar-mode config          -*- lexical-binding: t; -*-


(defun my/switch-project-in-new-tab ()
  (interactive)
  (let (succ)
    (unwind-protect
        (progn
          (tab-bar-new-tab)
          (call-interactively #'project-switch-project)
          (when-let ((proj (project-current)))
            (tab-bar-rename-tab (format "%s" (file-name-nondirectory (directory-file-name (cdr proj)))))
            (setq succ t)))
      (unless succ
        (tab-bar-close-tab)))))

(global-set-key [remap project-switch-project] #' my/switch-project-in-new-tab)

(with-eval-after-load 'tab-bar
  (setq-default tab-bar-border 5
                tab-bar-close-button nil
                tab-bar-back-button nil
                tab-bar-show nil
                tab-bar-new-button nil
                tab-bar-tab-hints t)

  (when (> emacs-major-version 27)
    (add-to-list 'tab-bar-format #'tab-bar-format-menu-bar))
  (setq tab-bar-new-tab-choice "*scratch*"))

(tab-bar-mode 1)

(provide 'init-tab-bar)
