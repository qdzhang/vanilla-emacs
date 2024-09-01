;;; shell-easy.el --- Use eshell or term easily      -*- lexical-binding: t; -*-

;;; Commentary:

;; Bind `shell-easy-transient-menu' to a key, then select what you want.'

;;; Code:

;; Helpers
;; Reference: https://github.com/mnewt/dotemacs/blob/master/init.el
(defvar switch-to-buffer-by-mode-history nil
  "History for `switch-to-buffer-by-mode'.")

(defun filter-buffers-by-mode (mode)
  "Return a list of buffers whose major mode is MODE."
  (when (stringp mode) (setq mode (intern mode)))
  (seq-filter (lambda (b) (eq (buffer-local-value 'major-mode b) mode))
              (buffer-list)))

(defun switch-to-buffer-by-mode (mode)
  "Choose a major MODE, then select from buffers of that mode."
  (interactive (list (if current-prefix-arg
                         (completing-read "Switch to buffers of mode:"
                                          (list-buffer-major-modes)
                                          nil t nil
                                          switch-to-buffer-by-mode-history)
                       major-mode)))
  (let ((buffers (mapcar #'buffer-name (filter-buffers-by-mode mode))))
    (switch-to-buffer (completing-read (format "%s buffers: " mode)
                                       buffers nil t nil
                                       switch-to-buffer-by-mode-history))))


;; Commands
(defun se-new-eshell ()
  "Open a new instance of eshell"
  (interactive)
  (eshell 'N))

(defun se-new-term (&optional name)
  "Start a new ternimal emulator using bash without confirming"
  (interactive)
  (ansi-term "/bin/bash" name))

(defun se-get-or-create-eshell ()
  "Get or create an eshell buffer."
  (interactive)
  (or (when current-prefix-arg (se-new-eshell))
      (car (filter-buffers-by-mode 'eshell-mode))
      (se-new-eshell)))

(defun se-get-or-create-term (&optional name)
  "Get or create an ansi-term buffer."
  (interactive)
  (or (when current-prefix-arg (se-new-term name))
      (car (filter-buffers-by-mode 'term-mode))
      (se-new-term name)))

(defun se-get-or-create-eshell-project-root ()
  "Get or create an eshell buffer in project root."
  (interactive)
  (let ((default-directory (or (cdr (project-current))
                               default-directory)))
    (or (when current-prefix-arg (se-new-eshell))
        (car (filter-buffers-by-mode 'eshell-mode))
        (se-new-eshell))))

(defun se-get-or-create-term-project-root ()
  "Get or create an ansi-term buffer in project root."
  (interactive)
  (let ((default-directory (or (cdr (project-current))
                               default-directory)))
    (or (when current-prefix-arg (se-new-term))
        (car (filter-buffers-by-mode 'term-mode))
        (se-new-term))))

(defun se-choose-eshell-buffer ()
  "Show all eshell buffer, and select to switch."
  (interactive)
  (switch-to-buffer-by-mode 'eshell-mode))

(defun se-choose-term-buffer ()
  "Show all term buffer, and select to switch."
  (interactive)
  (switch-to-buffer-by-mode 'term-mode))

(defun se-toggle-buffer-eshell ()
  "Switch to a recent eshell buffer or create a new one if not exists."
  (interactive)
  (if (derived-mode-p 'eshell-mode)
      (bury-buffer)
    (switch-to-buffer (se-get-or-create-eshell))))

(defun se-toggle-buffer-term ()
  "Switch to a recent term buffer or create a new one if not exists."
  (interactive)
  (if (derived-mode-p 'term-mode)
      (bury-buffer)
    (switch-to-buffer (se-get-or-create-term))))

(defun se-toggle-buffer-eshell-project-root ()
  "Switch to a recent eshell buffer or create a new one in the root of project
if not exists."
  (interactive)
  (if (derived-mode-p 'eshell-mode)
      (bury-buffer)
    (switch-to-buffer (se-get-or-create-eshell-project-root))))

(defun se-toggle-buffer-term-project-root ()
  "Switch to a recent term buffer or create a new one in the root of project if
not exists."
  (interactive)
  (if (derived-mode-p 'term-mode)
      (bury-buffer)
    (switch-to-buffer (se-get-or-create-term-project-root))))

(defun se-rename-buffer (name)
  (interactive "sName? ")
  (if (or (eq major-mode 'term-mode)
          (eq major-mode 'eshell-mode))
      (rename-buffer (format "*term*<%s>" name))
    (error "Not a shell buffer")))

(add-to-list 'display-buffer-alist
             '("\\*quake-term\\*"
               (display-buffer-in-side-window)
               (side . bottom)
               (slot . -1)
               (window-height . 0.3)))

(defun se-make-quake ()
  "Make current term to quake-like below the window."
  (interactive)
  (if (eq major-mode 'term-mode)
      (rename-buffer "*quake-term*")
    (error "Not a term buffer")))

(transient-define-prefix shell-easy-transient-menu ()
  "A transient menu for shell easy."
  [["Eshell"
    ("e" "eshell" se-toggle-buffer-eshell)
    ("P" "eshell project" se-toggle-buffer-eshell-project-root)
    ("N" "eshell new" se-new-eshell)]
   ["Ansi-term"
    ("t" "ansi-term" se-toggle-buffer-term)
    ("p" "ansi-term project" se-toggle-buffer-term-project-root)
    ("n" "anti-term new" se-new-term)]
   ["Quake-like"
    ("q" "quake" se-make-quake)
    ("`" "toggle quake" window-toggle-side-windows)]
   ["Navigation"
    ("<up>" "previous" previous-buffer)
    ("<down>" "next" next-buffer)
    ("r" "rename" se-rename-buffer)
    ("l" "choose ansi-term" se-choose-term-buffer)
    ("L" "choose eshell" se-choose-eshell-buffer)]])

(provide 'shell-easy)
;;; shell-easy.el ends here
