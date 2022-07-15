;;; init-tempo.el --- Config tempo                   -*- lexical-binding: t; -*-

;;; Commentary:

;; Config some useful tempo template

;;; Code:

(require 'tempo)

(setq tempo-interactive t)

(tempo-define-template
 "markdown-frontmatter"
 '("---" n
   "title: " "\"" (p "title: " title) "\"" n
   "date: " (format-time-string "%Y-%m-%d") n
   "lastmode:" n
   "description:" n
   "tags: [" (p "tags: " tags) "]" n
   "categories: [" (s tags) "]" n
   "series: []" n
   "toc: false" n
   "math: false" n
   "markup: md" n
   "draft: true" n
   "---" n)
 "frontmatter"
 "Insert yaml-style frontmatter before markdown post")

(tempo-define-template
 "shebang for env"
 '("#!/usr/bin/env ")
 "env"
 "Insert shebang env")

(tempo-define-template
 "shebang for bash"
 '("#!/bin/bash")
 "bash"
 "Insert shebang bash")

(tempo-define-template
 "shebang for sh"
 '("#!/bin/sh")
 "sh"
 "Insert shebang sh")

(tempo-define-template
 "shebang for perl"
 '("#!/usr/bin/perl")
 "perl"
 "Insert shebang perl")

(defun try-tempo-complete-tag (old)
  (unless old
    (tempo-complete-tag)))

(add-to-list 'hippie-expand-try-functions-list 'try-tempo-complete-tag)

(provide 'init-tempo)
;;; init-tempo.el ends here
