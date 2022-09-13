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
    (setf (buffer-substring start-pos end-pos) command)
    (end-of-line)))

(add-hook 'eshell-mode-hook (lambda ()
                              (setq completion-ignore-case t)
                              (setq read-file-name-completion-ignore-case t)
                              (setq read-buffer-completion-ignore-case t)
                              (setq eshell-buffer-maximum-lines 20000
                                    eshell-history-size 350
                                    eshell-hist-ignoredups t)))

(defalias 'e 'eshell)
(defalias 'at 'ansi-term)
(defalias 'eshell/f 'find-file)
(defalias 'eshell/x 'eshell/exit)
(defalias 'eshell/v 'view-file)
(defalias 'eshell/d 'dired-jump)

(defun with-face (str &rest face-plist)
  (propertize str 'face face-plist))

(defun fish-path (path max-len)
  "Return a potentially trimmed-down version of the directory PATH, replacing
parent directories with their initial characters to try to get the character
length of PATH (sans directory slashes) down to MAX-LEN."
  (let* ((components (split-string (abbreviate-file-name path) "/"))
         (len (+ (1- (length components))
                 (cl-reduce '+ components :key 'length)))
         (str ""))
    (while (and (> len max-len)
                (cdr components))
      (setq str (concat str
                        (cond ((= 0 (length (car components))) "/")
                              ((= 1 (length (car components)))
                               (concat (car components) "/"))
                              (t
                               (if (string= "."
                                            (string (elt (car components) 0)))
                                   (concat (substring (car components) 0 2)
                                           "/")
                                 (string (elt (car components) 0) ?/)))))
            len (- len (1- (length (car components))))
            components (cdr components)))
    (concat str (cl-reduce (lambda (a b) (concat a "/" b)) components))))

(defun shk-eshell-prompt ()
  (let ((blue "DodgerBlue"))
    (concat
     (with-face (concat (fish-path (eshell/pwd) 40) " ")
                :foreground blue)
     (when (bound-and-true-p socks-noproxy)
       (with-face "[proxy] " :foreground "purple"))
     (with-face
      (or (ignore-errors (format "(%s)" (vc-responsible-backend default-directory))) ""))
     (when (featurep 'chruby)
       (with-face (concat " <"
                          (or (chruby-current)
                              "default-ruby")
                          ">")
                  :foreground "SpringGreen"))
     (with-face "\n" :foreground blue)
     (if (= (user-uid) 0)
         (with-face "#" :foreground "red")
       "λ")
     " ")))
(setq eshell-prompt-function 'shk-eshell-prompt)
(setq eshell-highlight-prompt nil)

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

;; Visual commands
(require 'em-term)
(mapc (lambda (x) (add-to-list 'eshell-visual-commands x))
      '("el" "elinks" "htop" "less" "ssh" "tmux" "top"))

;; Copied from Spacemacs shell layer
(defun my/protect-eshell-prompt ()
  "Protect Eshell's prompt like Comint's prompts.
E.g. `evil-change-whole-line' won't wipe the prompt. This
is achieved by adding the relevant text properties."
  (let ((inhibit-field-text-motion t))
    (add-text-properties
     (point-at-bol)
     (point)
     '(rear-nonsticky t
                      inhibit-line-move-field-capture t
                      field output
                      read-only t
                      front-sticky (field inhibit-line-move-field-capture)))))

(add-hook 'eshell-after-prompt-hook 'my/protect-eshell-prompt)

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

(provide 'init-eshell)
