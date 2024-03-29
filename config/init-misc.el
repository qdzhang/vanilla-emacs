;;; init-misc.el -*- lexical-binding: t; -*-

(progn
  ;; Save minibuffer history
  (savehist-mode 1)

  ;; Save kill-ring history
  (add-to-list 'savehist-additional-variables 'kill-ring)

  (autoload 'zap-up-to-char "misc"
    "Kill up to, but not including ARGth occurrence of CHAR." t)

  ;; Fix CJK word wrap
  ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=29364
  (setq-default word-wrap-by-category t)

  (setq-default indent-tabs-mode nil)
  (setq save-interprogram-paste-before-kill t
        apropos-do-all t
        mouse-yank-at-point t
        require-final-newline t
        load-prefer-newer t
        ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function #'split-window-horizontally
        custom-file (expand-file-name "~/.emacs.d/custom.el"))

  ;; Use font caches to preventing some GC
  (setq inhibit-compacting-font-caches t)

  (unless backup-directory-alist
    (setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                                   "backups")))))

  ;; Optimization for very long file
  ;; https://emacs-china.org/t/topic/25811/9
  (setq-default bidi-display-reordering nil)
  (setq bidi-inhibit-bpa t
        long-line-threshold 1000
        large-hscroll-threshold 1000
        syntax-wholeline-max 1000))

;;; When use FIND-FILE to create a new file, if the directory doesn't exist yet,
;;; create the directory
;;; https://superuser.com/questions/131538/can-i-create-directories-that-dont-exist-while-creating-a-new-file-in-emacs
;; Formerly I am using advices to achieve this like the answer in previous link.
;; But adding a procedure for `find-file-not-found-functions' hook is simpler.
(defun my/auto-create-missing-dirs ()
  "Create the directory if new file is created in a nonexist directory.

URL: https://emacsredux.com/blog/2022/06/12/auto-create-missing-directories/"
  (let ((target-dir (file-name-directory buffer-file-name)))
    (unless (file-exists-p target-dir)
      (make-directory target-dir t))))

(add-to-list 'find-file-not-found-functions #'my/auto-create-missing-dirs)


(provide 'init-misc)
