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
                              (setq read-buffer-completion-ignore-case t)))

(defalias 'e 'eshell)
(defalias 'at 'ansi-term)


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
     (with-face
      (or (ignore-errors (format "(%s)" (vc-responsible-backend default-directory))) ""))
     (when (featurep 'chruby)
       (with-face (concat " <"
                          (chruby-current)
                          ">")
                  :foreground "SpringGreen"))
     (with-face "\n" :foreground blue)
     (if (= (user-uid) 0)
         (with-face "#" :foreground "red")
       "λ")
     " ")))
(setq eshell-prompt-function 'shk-eshell-prompt)
(setq eshell-highlight-prompt nil)


(provide 'init-eshell)
