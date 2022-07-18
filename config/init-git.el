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

(defun my/add-current-file-to-dotfiles ()
  "Add current file to dotfiles respository"
  (interactive)
  (let* ((command "dotfiles add ")
         (file-name (buffer-file-name))
         (full-command (concat command file-name))
         (shell-command-switch "-ic"))
    (start-file-process-shell-command "add-to-dotfiles" "*Messages*" full-command)
    (message "Add current file to dotfiles")))

(transient-define-prefix my-transient/magit-log ()
  "Log"
  [["File"
    ("f" "Current" magit-log-buffer-file)
    ("F" "File Popup" magit-log)
    ("l" "Log All" magit-log-all)
    ("u" "Unmerged Commits" magit-cherry)]
   ["Branch"
    ("p" "Pick..." magit-log-other)
    ("c" "Current" magit-log-current)
    ("h" "Head" magit-log-head)
    ("o" "Local & Head" magit-log-branches)
    ("a" "Local & Head & Remote" magit-log-all-branches)
    ("A" "Everything" magit-log-all)]
   ["Reflog"
    ("P" "Pick..." magit-reflog-other)
    ("C" "Current" magit-reflog-current)
    ("H" "Head" magit-reflog-head)]])

(transient-define-prefix my-transient/ediff ()
  "Ediff"
  [["Actions"
    ("f" "Files" ediff-files)
    ("F" "Files - (3 Way)" ediff-files3)
    ("b" "Buffers" ediff-buffers)
    ("B" "Buffers - (3 Way)" ediff-buffers3)
    ("d" "Directories" ediff-directories)
    ("D" "Directories - (3 Way)" ediff-directories3)]])

(transient-define-prefix my-transient/magit-menu ()
  "Magit transient menu"
  [["Repository"
    ("s" "status" magit-status)
    ("c" "checkout" magit-checkout)
    ("C" "commit" magit-commit)
    ("D" "Magit discarded" magit-discard)
    ("," "Magit init" magit-init)
    ("." "Magit add remote" magit-remote-add)
    ("r" "rebase" magit-rebase)
    ("b" "branch" magit-branch)
    ("o" "submodule" magit-submodule)]
   ["History"
    ("l" "Log" my-transient/magit-log)
    ("e" "Ediff" my-transient/ediff)
    ("j" "Blob next" magit-blob-next)
    ("k" "Blob previous" magit-blob-previous)]
   ["Files"
    ("p" "File Popup" magit-file-dispatch)
    ("f" "Find File" magit-find-file)
    ("F" "Find File in Other Window" magit-find-file-other-window)]
   ["Dotfiles"
    ("d" "Dotfiles" my/magit-dotfiles-status)
    ("a" "Add dotfiles" my/add-current-file-to-dotfiles)]])

(provide 'init-git)
