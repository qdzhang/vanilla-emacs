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

(transient-define-prefix my-transient/tab-bar-menu ()
  "Tab-bar mode transient menu"
  [["Manage"
    ("0" "Close" tab-close)
    ("1" "Close(other)" tab-close-other)
    ("2" "New" tab-new)
    ("G" "Group" tab-group)
    ("M" "Move to..." tab-move-to)
    ("N" "New to..." tab-new-to)
    ("m" "Move" tab-move)
    ("n" "Duplicate" tab-duplicate)
    ("r" "Rename" tab-rename)
    ("u" "Undo" tab-undo)]
   ["Switch"
    ("O" "Previous" tab-previous)
    ("o" "Next" tab-next)
    ("[" "Previous(same)" tab-bar-switch-to-prev-tab)
    ("]" "Next(same)" tab-bar-switch-to-next-tab)
    ("l" "List tab" tab-bar-switch-to-tab)
    ("b" "Switch to buffer(other tab)" switch-to-buffer-other-tab)]
   ["Other"
    ("d" "Dired" dired-other-tab)
    ("f" "Find file" find-file-other-tab)
    ("p" "Project" my-transient/project-menu)]])

(provide 'init-tab-bar)
