;;; init-git.el --- Init for git     -*- lexical-binding: t; -*-

(with-eval-after-load 'magit
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-stashes
                          'append))

;; Make magit status show full screen
;; https://github.com/magit/magit/issues/1953#issuecomment-221134023
(setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)

;; This function can make magit working in home directory
;; with a bare git respository,such as dotfiles directory
(defun ~/magit-process-environment (env)
  "Add GIT_DIR and GIT_WORK_TREE to ENV when in a special directory.
https://github.com/magit/magit/issues/460 (@cpitclaudel)."
  (let ((default (file-name-as-directory (expand-file-name default-directory)))
        (home (expand-file-name "~/")))
    (when (string= default home)
      (let ((gitdir (expand-file-name "~/.dotfiles/")))
        (push (format "GIT_WORK_TREE=%s" home) env)
        (push (format "GIT_DIR=%s" gitdir) env))))
  env)

(advice-add 'magit-process-environment
            :filter-return #'~/magit-process-environment)

(defun my/magit-dotfiles-status ()
  "Show magit status in home directory containing dotfiles"
  (interactive)
  (magit-status "~/"))

(one-key-create-menu
 "MAGIT"
 '(
   (("s" . "Magit status") . magit-status)
   (("c" . "Magit checkout") . magit-checkout)
   (("C" . "Magit commit") . magit-commit)
   (("u" . "Magit push to remote") . magit-push-current-to-pushremote)
   (("p" . "Magit delete remote branch") . magit-delete-remote-branch)
   (("i" . "Magit pull") . magit-pull-from-upstream)
   (("r" . "Magit rebase") . magit-rebase)
   (("e" . "Magit merge") . magit-merge)
   (("l" . "Magit log") . magit-log-all)
   (("L" . "Magit blame") . magit-blame+)
   (("b" . "Magit branch") . magit-branch)
   (("B" . "Magit buffer") . magit-process-buffer)
   (("D" . "Magit discarded") . magit-discard)
   (("d" . "Dotfiles") . my/magit-dotfiles-status)
   (("," . "Magit init") . magit-init)
   (("." . "Magit add remote") . magit-remote-add)
   )
 t)

(provide 'init-git)
