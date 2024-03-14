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

(defun se-new-term ()
  "Start a new ternimal emulator using bash without confirming"
  (interactive)
  (ansi-term "/bin/bash"))

(defun se-get-or-create-eshell ()
  "Get or create an eshell buffer."
  (interactive)
  (or (when current-prefix-arg (se-new-eshell))
      (car (filter-buffers-by-mode 'eshell-mode))
      (se-new-eshell)))

(defun se-get-or-create-term ()
  "Get or create an ansi-term buffer."
  (interactive)
  (or (when current-prefix-arg (se-new-term))
      (car (filter-buffers-by-mode 'term-mode))
      (se-new-term)))

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
      (previous-buffer)
    (switch-to-buffer (se-get-or-create-eshell))))

(defun se-toggle-buffer-term ()
  "Switch to a recent term buffer or create a new one if not exists."
  (interactive)
  (if (derived-mode-p 'term-mode)
      (previous-buffer)
    (switch-to-buffer (se-get-or-create-term))))

(defun se-toggle-buffer-eshell-project-root ()
  "Switch to a recent eshell buffer or create a new one in the root of project
if not exists."
  (interactive)
  (if (derived-mode-p 'eshell-mode)
      (previous-buffer)
    (switch-to-buffer (se-get-or-create-eshell-project-root))))

(defun se-toggle-buffer-term-project-root ()
  "Switch to a recent term buffer or create a new one in the root of project if
not exists."
  (interactive)
  (if (derived-mode-p 'term-mode)
      (previous-buffer)
    (switch-to-buffer (se-get-or-create-term-project-root))))


(transient-define-prefix shell-easy-transient-menu ()
  "A transient menu for shell easy."
  [["Eshell"
    ("e" "eshell" se-toggle-buffer-eshell)
    ("p" "eshell project" se-toggle-buffer-eshell-project-root)]
   ["Ansi-term"
    ("t" "ansi-term" se-toggle-buffer-term)
    ("f" "ansi-term project" se-toggle-buffer-term-project-root)]
   ["Navigation"
    ("<up>" "previous" previous-buffer)
    ("<down>" "next" next-buffer)
    ("`" "choose eshell" se-choose-eshell-buffer)
    ("c" "choose ansi-term" se-choose-term-buffer)]])

(provide 'shell-easy)
;;; shell-easy.el ends here
