;;; sane-eshell.el --- Multi Eshell is crazy. This is not.  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 qdzhang
;; Copyright (C) 2018 Adam Patterson

;; Modified from `sane-term':
;; URL: http://github.com/adamrt/sane-term
;; Original Author: Adam Patterson <adam@adamrt.com>

;; Package-Requires: ((emacs "24.1"))

;;; Commentary:

;; Open multi eshells and cycle in theme
;;
;; There are two main functions:
;; - `sane-eshell': If there is no eshell yet, create a new one; otherwise cycle
;;   between all eshell buffers
;; - `sane-eshell-create': Create a new eshell buffer named "eshell<N>"(N is a
;;   positive number)
;; - `sane-eshell-create-in-project-root': Create a new eshell in the root of
;;   current project

;;; Code:

(defgroup sane-eshell nil
  "Multi Eshell is crazy. This is not."
  :group 'eshell)

(defcustom sane-eshell-initial-create t
  "Creates a eshell if one doesn't exist."
  :type 'boolean
  :group 'sane-eshell)

(defun sane-eshell-buffer-exists-p ()
  "Boolean if eshell-mode buffers exist."
  (catch 'loop
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (derived-mode-p 'eshell-mode)
          (throw 'loop t))))))

(defun sane-eshell-cycle (reverse)
  (unless reverse
    (when (derived-mode-p 'eshell-mode)
      (bury-buffer)))
  (let ((buffers (buffer-list)))
    (when reverse
      (setq buffers (nreverse buffers)))
    (catch 'loop
      (dolist (buf buffers)
        (when (with-current-buffer buf (derived-mode-p 'eshell-mode))
          (switch-to-buffer buf)
          (throw 'loop nil))))))

(defun sane-eshell-prev ()
  "Cycle through eshell buffers, in reverse."
  (interactive)
  (sane-eshell-cycle t))

(defun sane-eshell-next ()
  "Cycle through eshell buffers."
  (interactive)
  (sane-eshell-cycle nil))

;;;###autoload
(defun sane-eshell-create ()
  "Create new eshell buffer."
  (interactive)
  (eshell 'N))

;;;###autoload
(defun sane-eshell-create-in-project-root ()
  "Create new eshell buffer in current project root"
  (interactive)
  (let ((default-directory (cdr (project-current))))
    (eshell 'N)))

;;;###autoload
(defun sane-eshell ()
  "Cycle through eshell buffers, creating if necessary."
  (interactive)
  (when sane-eshell-initial-create
    (unless (sane-eshell-buffer-exists-p)
      (sane-eshell-create)))
  (sane-eshell-next))

(provide 'sane-eshell)

;;; sane-eshell.el ends here
