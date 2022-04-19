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
                tab-bar-new-button nil
                tab-bar-tab-hints t)
  
  ;; Config `tab-bar-tab-name-format-function' to show pretty numbers
  ;; https://christiantietze.de/posts/2022/02/emacs-tab-bar-numbered-tabs/
  (defvar ct/circle-numbers-alist
    '((0 . "⓪")
      (1 . "①")
      (2 . "②")
      (3 . "③")
      (4 . "④")
      (5 . "⑤")
      (6 . "⑥")
      (7 . "⑦")
      (8 . "⑧")
      (9 . "⑨"))
    "Alist of integers to strings of circled unicode numbers.")

  (defun ct/tab-bar-tab-name-format-default (tab i)
    (let* ((current-p (eq (car tab) 'current-tab))
           (tab-num (if (and tab-bar-tab-hints (< i 10))
                        (alist-get i ct/circle-numbers-alist) ""))
           (tab-name (alist-get 'name tab))
           (tab-name-length (length tab-name))
           (tab-name-too-long? (> tab-name-length 20))
           (tab-shorten-name (if tab-name-too-long?
                                 (concat (substring tab-name 0 15)
                                         "..."
                                         (substring tab-name (- tab-name-length 7) tab-name-length))
                               tab-name)))
      (propertize
       (concat tab-num
               " "
               tab-shorten-name
               (or (and tab-bar-close-button-show
                        (not (eq tab-bar-close-button-show
                                 (if current-p 'non-selected 'selected)))
                        tab-bar-close-button)
                   "")
               " ")
       'face (funcall tab-bar-tab-face-function tab))))

  (setq tab-bar-tab-name-format-function #'ct/tab-bar-tab-name-format-default)

  (when (> emacs-major-version 27)
    (add-to-list 'tab-bar-format #'tab-bar-format-menu-bar))
  (setq tab-bar-new-tab-choice "*scratch*"))

(tab-bar-mode 1)

(provide 'init-tab-bar)
