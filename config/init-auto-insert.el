;;; init-auto-insert.el --- Config auto-insert       -*- lexical-binding: t; -*-

;;; Commentary:

;; Config `auto-insert' and skeleton

;;; Code:

(setq auto-insert-query nil)

(define-auto-insert "\\.el$" 'my/elisp-header)

(define-skeleton my/elisp-header
  "Insert my custom elisp header"
  "Short description: "
  ";;; " (file-name-nondirectory (buffer-file-name))
  " --- " str (make-string (max 2 (- 80 (current-column) 27)) 32)
  "-*- lexical-binding: t; -*-"
  '(setq lexical-binding t)
  "

;; Copyright (C) " (format-time-string "%Y") "  " (user-full-name)
  "

;; Author: " (user-full-name)
  '(if
       (search-backward "&"
                        (line-beginning-position)
                        t)
       (replace-match
        (capitalize
         (user-login-name))
        t t))
  '(end-of-line 1)
  " <"
  (progn user-mail-address)
  ">
;; Maintainer: " (user-full-name)
  '(if
       (search-backward "&"
                        (line-beginning-position)
                        t)
       (replace-match
        (capitalize
         (user-login-name))
        t t))
  '(end-of-line 1)
  " <"
  (progn user-mail-address)
  ">
;; Created: " (format-time-string "%e %B %Y")
  "
;; URL:
;; Version: 0.1
;; Keywords:
;; Package-Requires: ()
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

" _ "

(provide '"(file-name-base (buffer-file-name))
  ")
;;; "
  (file-name-nondirectory
   (buffer-file-name))
  " ends here")

(define-skeleton my/simple-elisp-header
  "This is a simple elisp header to write my config files"
  "Short description: "
  ";;; " (file-name-nondirectory (buffer-file-name))
  " --- " str (make-string (max 2 (- 80 (current-column) 27)) 32)
  "-*- lexical-binding: t; -*-"
  '(setq lexical-binding t)
  "

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

  " _ "

  (provide '"(file-name-base (buffer-file-name))
  ")
;;; "
  (file-name-nondirectory
   (buffer-file-name)))

(define-skeleton insert-c-comment-rectangle
  "Inserts a c comment in a rectangle into current buffer."
  ""
  '(setq str (skeleton-read "Comment: "))
  ;; `str' is set explicitly here, because otherwise the skeleton
  ;; program would set it, only when it is going to insert it into the
  ;; buffer. But we need to determine the length of the string
  ;; beforehand, with `(length str)' below.
  '(when (string= str "") (setq str " - "))
  '(setq v1 (make-string (- fill-column 6) ?*))
  '(setq v2 (- fill-column 10 (length str)))
  "/* " v1 " */" \n
  "/* **"
  (make-string (floor v2 2) ?\ )
  str
  (make-string (ceiling v2 2) ?\ )
  "** */" \n
  "/* " v1 " */")

(define-skeleton insert-lisp-comment-rectangle
  "Insert a lisp comment in a rectangle into current buffer."
  ""
  '(setq str (skeleton-read "Comment: "))
  '(when (string= str "") (setq str " - "))
  '(setq v1 (make-string (- fill-column 6) ?-))
  '(setq v2 (- fill-column 10 (length str)))
  ";;; +" v1 "+" \n
  ";;  |"
  (make-string (floor v2 2) ?\ )
  str
  (make-string (ceiling v2 2) ?\ )
  "    |" \n
  ";;; +" v1 "+")

(define-skeleton my/autoload
  "Insert autoload magic comment"
  ""
  ";;;###autoload")

(provide 'init-auto-insert)
;;; init-auto-insert.el ends here
