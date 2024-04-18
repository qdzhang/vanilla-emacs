;;; init-eshell.el --- eshell config                 -*- lexical-binding: t; -*-

(defun my/eshell-history ()
  (interactive)
  (require 'em-hist)
  (let* ((start-pos (save-excursion (eshell-bol) (point)))
         (end-pos (point))
         (input (buffer-substring-no-properties start-pos end-pos))
         (command (completing-read "History: "
                                   (when (> (ring-size eshell-history-ring) 0)
                                     (ring-elements eshell-history-ring)))))
    (setf (buffer-substring-no-properties start-pos end-pos) command)
    (end-of-line)))

;; Config eshell prompt
(setq eshell-prompt-regexp "^[^#$\n]*[#$] "
      eshell-prompt-function
      (lambda nil
        (concat
         (if (string= (eshell/pwd) (getenv "HOME"))
             "~"
           (eshell/basename (eshell/pwd)))
         " "
         (if (= (user-uid) 0) "# " "$ "))))

(add-hook 'eshell-mode-hook (lambda ()
                              (setq completion-ignore-case t)
                              (setq read-file-name-completion-ignore-case t)
                              (setq read-buffer-completion-ignore-case t)
                              (setq eshell-cmpl-ignore-case t)
                              (setq eshell-cmpl-cycle-completions nil)
                              (setq eshell-buffer-maximum-lines 20000
                                    eshell-history-size 350
                                    eshell-hist-ignoredups t)))

(defalias 'eshell/f 'find-file)
(defalias 'eshell/x 'eshell/exit)
(defalias 'eshell/v 'view-file)
(defalias 'eshell/d 'dired-jump)

(defun eshell/unpack (file &rest args)
  "Unpack FILE with ARGS using default command."
  (let ((command (cl-some (lambda (x)
                            (if (string-match-p (car x) file)
                                (cadr x)))
                          '((".*\.tar.bz2" "tar xjf")
                            (".*\.tar.gz" "tar xzf")
                            (".*\.bz2" "bunzip2")
                            (".*\.rar" "unrar x")
                            (".*\.gz" "gunzip")
                            (".*\.tar" "tar xf")
                            (".*\.tbz2" "tar xjf")
                            (".*\.tgz" "tar xzf")
                            (".*\.zip" "unzip")
                            (".*\.rar" "unrar x")
                            (".*\.Z" "uncompress")
                            (".*" "echo 'Could not unpack the file:'")))))
    (let ((unpack-command (concat command " " file " " (mapconcat 'identity args " "))))
      (eshell/printnl "Unpack command: " unpack-command)
      (eshell-command-result unpack-command))))

(defun eshell/ccat (file)
  "Like `cat' but output with Emacs syntax highlighting.

Ref: https://codeberg.org/vifon/emacs-config/src/branch/master/emacs.d/lisp/30-eshell.el"
  (with-temp-buffer
    (insert-file-contents file)
    (let ((buffer-file-name file))
      (delay-mode-hooks
        (set-auto-mode)
        (if (fboundp 'font-lock-ensure)
            (font-lock-ensure)
          (with-no-warnings
            (font-lock-fontify-buffer)))))
    (buffer-string)))

;; https://github.com/condy0919/.emacs.d/commit/8519a2af5847ecb69ff841db8ef76ed42465fb80
(defun eshell/rg (&rest args)
  "ripgrep with eshell integration."
  (eshell-grep "rg" (append '("--no-heading") args) t))

;; Visual commands
(require 'em-term)
(mapc (lambda (x) (add-to-list 'eshell-visual-commands x))
      '("el" "elinks" "htop" "less" "ssh" "tmux" "top"))

(defun my/kill-word-backward ()
  "Let Eshell kill word acting like zsh."
  (interactive)
  (set-mark-command nil)
  (backward-word)
  (call-interactively 'kill-region))

;; Copied from Spacemacs
(defun my/eshell-clear-keystroke ()
  "Allow for keystrokes to invoke eshell/clear"
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (eshell-send-input))

(defun my/eshell-xdg-open (file)
  "Invoke `xdg-open' to open file in eshell"
  (interactive)
  (let ((command (format "xdg-open '%s'" file)))
    (shell-command command)))

(defalias 'eshell/o 'my/eshell-xdg-open)

;; https://stackoverflow.com/a/51867960
(defun my/eshell-exit-close-window ()
  "When exit `eshell', close the window."
  (when (not (one-window-p))
    (delete-window)))

(advice-add 'eshell-life-is-too-much :after 'my/eshell-exit-close-window)

(provide 'init-eshell)
