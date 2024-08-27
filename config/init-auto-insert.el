;;; init-auto-insert.el --- Config auto-insert       -*- lexical-binding: t; -*-

;;; Commentary:

;; Config `auto-insert' and skeleton

;;; Code:

(setq auto-insert-query nil)

(setq skeleton-end-newline nil)

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

(define-skeleton my/meson-build-simple-template
  "A simple template for meson.build file"
  "Project name: "
  "project('" str "', '"
  (setq v1 (skeleton-read "Language: (c/cpp) "))
  "')" \n
  "add_project_arguments('-fanalyzer', language : '" v1 "')" \n
  \n
  "executable('"
  (skeleton-read "Output file: ")
  "', '"
  (skeleton-read "Source file: ")
  "')"\n)

(define-auto-insert "/meson.build\\'" 'my/meson-build-simple-template)

(define-skeleton my-skel/html5
  "Basic html5 file"
  "Title: "
  "<!DOCTYPE html>
<html lang=\"en\">
  <head>
    <meta charset=\"utf-8\" />
    <meta http-equiv=\"x-ua-compatible\" content=\"ie=edge\" />
    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\" />

    <title>" str "</title>
    <link rel=\"stylesheet\" href=\"css/main.css\" />
    <link rel=\"icon\" href=\"images/favicon.png\" />
  </head>

  <body>
    <h1>" str "</h1>
  </body>
</html>
")

(define-skeleton my-skel/hugo-go-template
  "Insert hugo template delimiter"
  ""
  "{{ " - " }}")

(define-skeleton my-skel/hugo-go-template-without-whitespaces
  "Insert hugo template delimiter without whitespaces"
  ""
  "{{- " - " -}}")

(define-skeleton my-skel/use-c++-mode
  "Insert comments to make current buffer use `c++-mode'"
  ""
  "// Local Variables:
// mode: c++
// End:")

;; https://gist.github.com/ctizen/e732e241760f3fc019ead1ee15ad08c8
(define-skeleton my-skel/tsdoc
  "Insert tsdoc"
  ""
  "/**
 * 
"
  (save-excursion
    (let* ((fPos
            (re-search-forward "\\<\\(function\\|public\\|private\\|protected\\)\\s-*\.*(\\([^)]*\\))"))
           (argStr (and fPos (match-string 2)))
           (argList (and fPos (split-string argStr ",\\s-*"))))
      (cl-loop with params = nil
               for arg in argList
               do
               (set 'argMatched (string-match "\\([^:]+\\)\\(: *\\(.+\\)\\)?$" arg))
               (set 'pName (match-string 1 arg))
               (set 'pType (match-string 3 arg))
               (set 'param (format " * @param {%s} %s " (if pType pType "") pName))
               (if (< 0 (length arg))
                   (push param params)
                 )
               finally return
               (mapconcat 'identity (reverse (cons " * @return " params)) "\n"))))
  "
 */
")

(define-skeleton my-skel/ruby-gem-script
  "Init ruby script with gemfiles"
  ""
  "#!/usr/bin/env ruby

require 'bundler/inline'

gemfile do
  source 'https://rubygems.org'
  # add gems you need here
end
")

(define-skeleton my-skel/clang-format
  ""
  "# See https://www.apertis.org/policies/coding_conventions/#code-formatting
BasedOnStyle: LLVM
AlwaysBreakAfterDefinitionReturnType: All
BreakBeforeBinaryOperators: None
BinPackParameters: false
SpaceAfterCStyleCast: true
PointerAlignment: Right
# Our column limit is actually 80, but setting that results in clang-format
# making a lot of dubious hanging-indent choices; disable it and assume the
# developer will line wrap appropriately. clang-format will still check
# existing hanging indents.
ColumnLimit: 0
")

(define-auto-insert "\\.clang-format$" 'my-skel/clang-format)


;; https://github.com/ahei/dea/blob/7cc40d7bb48aed26ed7e1e681f626454cce98a81/my-lisps/c-settings.el
(defconst system-head-file-dir
  (list "/usr/include" "/usr/local/include" "/usr/include/sys") "System head files")
(defconst user-head-file-dir
  (list "." (if (file-directory-p "../include") "../include" "")) "User head files")

(define-skeleton my-skel/include-system
  "insert #include<>"
  ""
  > "#include <"
  (completing-read "System head files: "
                   (mapcar #'(lambda (f) (list f ))
                           (apply 'append (mapcar #'(lambda (dir) (directory-files dir))
                                                  system-head-file-dir)))) ">\n")

(define-skeleton my-skel/include-user
  "insert #include\"\""
  ""
  > "#include \""
  (completing-read "User head files: "
                   (mapcar #'(lambda (f) (list f ))
                           (apply 'append (mapcar #'(lambda (dir) (directory-files dir))
                                                  user-head-file-dir)))) "\"\n")

(define-skeleton my-skel/c-simple-header
  "insert a simple description header comment in c-mode"
  "Short description: "
  "/* " (file-name-nondirectory (buffer-file-name))
  " -- " str (make-string (max 2 (- 80 (current-column) 27)) 32) "*/
")

(define-skeleton my-skel/org-header
  "insert header of org-mode"
  "title: "
  "#+TITLE: " str \n
  "#+AUTHOR: " user-full-name \n
  "#+DATE: " (my/time-stamp) \n)

(define-auto-insert "\\.org$" 'my-skel/org-header)

(provide 'init-auto-insert)
;;; init-auto-insert.el ends here
