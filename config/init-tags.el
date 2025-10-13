;;; init-tags.el --- Config tags systems             -*- lexical-binding: t; -*-

;;; Commentary:

;; Config etags, ctags, global and citre

;;; Code:

(defun my/c-project-create-etags (dir-name)
  "Create etags file in a c project."
  (interactive "DDirectory: ")
  (eshell-command
   ;; (format "find %s -type f -name \"*.c\" -o -name \"*.h\" -o -name \"*.cpp\" -o -name \"*.hpp\" | etags --include \"~/tags/TAGS\"  -" dir-name)
   (format "find %s -type f -name \"*.c\" -o -name \"*.h\" -o -name \"*.cpp\" -o -name \"*.hpp\" | etags -" dir-name)))

(defun my/ruby-project-create-ctags ()
  "Create ctags file in a ruby project."
  (interactive)
  (shell-command "ctags -R --languages=ruby --exclude=.git --exclude=log --exclude=tmp . $(bundle list --paths)"))

(defun my/perl-project-create-ctags ()
  "Create ctags file in a perl project."
  (interactive)
  (shell-command "ctags -R --languages=Perl --exclude=.git --exclude=log --exclude=tmp"))

;; Use global TAGS file
;; (setq tags-table-list '("TAGS" "~/tags/TAGS"))

;; Setup citre
;;
;; (require 'citre)
;; (require 'citre-config)

;; Setup for rbtagger
;;
;; Make tag search case sensitive. Highly recommended for
;; maximum precision.
(setq tags-case-fold-search nil)

;; Reread TAGS without querying if it has changed
(setq tags-revert-without-query 1)

;; Always start a new tags list (do not accumulate a list of
;; tags) to keep up with the convention of one TAGS per project.
(setq tags-add-tables nil)

;; Setup global
;; Use `gtags-gen' to create Global tags, and after save hook to update
;; GLOBAL database with changed data.

(require 'gtags)

(add-hook 'gtags-mode-hook #'my/gtags-mode-keybindings)

(defun my/gtags-mode-keybindings ()
  "keybindings for `gtags-mode'."
  (define-key gtags-mode-map (kbd "M-.") 'gtags-find-tag)
  (define-key gtags-mode-map (kbd "M-,") 'gtags-pop-stack)
  (define-key gtags-mode-map (kbd "M-?") 'gtags-find-rtag)
  (define-key gtags-mode-map (kbd "C-M-.") 'gtags-find-pattern)
  (define-key gtags-mode-map (kbd "C-M-;") 'gtags-next-gtag))


(defun my/gtags-select-mode-keybindings ()
  "keybindings for `gtags-select-mode'."
  (define-key gtags-select-mode-map (kbd "q") 'gtags-pop-stack)
  (define-key gtags-select-mode-map (kbd "n") 'next-line)
  (define-key gtags-select-mode-map (kbd "p") 'previous-line))


;; Setting to make `gtags-select-mode' easy to see
;;
(add-hook 'gtags-select-mode-hook (lambda ()
                                    (hl-line-mode 1)
                                    (my/gtags-select-mode-keybindings)))


;; https://github.com/surki/dotemacs/blob/master/init.org
(defun gtags-gen ()
  "Create the gnu global tag file. If tags files exist, update them."
  (interactive)
  (let ((default-directory (gtags-root-dir)))
    (if (zerop (call-process "global" nil nil nil " -p")) ; tagfile doesn't exist?
        (progn
          (start-process "gtags" "*Messages*" "global" "--update")
          (message "Gtags update done."))
      (start-process "gtags" "*Messages*" "gtags")
      (message "Gtags create done."))))

(defalias 'ggen 'gtags-gen)

(defun gtags-root-dir ()
  "Returns GTAGS root directory or nil if doesn't exist."
  (with-temp-buffer
    (if (zerop (call-process "global" nil t nil "-pr"))
        (buffer-substring (point-min) (1- (point-max)))
      (project-root (project-current)))))

(defun gtags-update-single(filename)
  "Update Gtags database for changes in a single file"
  (interactive)
  (if (eq system-type 'windows-nt)
      (start-process "update-gtags" "update-gtags" "cmd" "/c" (concat "cd " (gtags-root-dir) " && gtags --single-update " filename ))
    (start-process "update-gtags" "update-gtags" "bash" "-c" (concat "cd " (gtags-root-dir) " ; gtags --single-update " filename ))))

(defun gtags-update-current-file()
  (interactive)
  (let ((gtagsfilename (replace-regexp-in-string (gtags-root-dir) "." (buffer-file-name (current-buffer)))))
    (gtags-update-single gtagsfilename)
    (message "Gtags updated for %s" gtagsfilename)))

(defun gtags-update-hook()
  "Update GTAGS file incrementally upon saving a file"
  (when gtags-mode
    (when (gtags-root-dir)
      (gtags-update-current-file))))


(defun gtags-next-gtag ()
  "Find next matching tag, for GTAGS."
  (interactive)
  (let ((latest-gtags-buffer
         (car (delq nil  (mapcar (lambda (x) (and (string-match "GTAGS SELECT" (buffer-name x)) (buffer-name x)) )
                                 (buffer-list)) ))))
    (cond (latest-gtags-buffer
           (switch-to-buffer latest-gtags-buffer)
           (next-line)
           (gtags-select-it nil)))))

(add-hook 'gtags-mode-hook
          (lambda ()
            (add-hook 'after-save-hook 'gtags-update-hook nil t)))


(provide 'init-tags)
;;; init-tags.el ends here
