;;; init-git.el --- Init for git

(with-eval-after-load 'magit
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-stashes
                          'append))


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
   (("," . "Magit init") . magit-init)
   (("." . "Magit add remote") . magit-remote-add)
   )
 t)

(provide 'init-git)
