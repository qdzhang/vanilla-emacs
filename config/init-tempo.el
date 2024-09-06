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
 "shebang-for-bash"
 '("#!/bin/bash")
 "bash"
 "Insert shebang bash")

(tempo-define-template
 "shebang-for-sh"
 '("#!/bin/sh")
 "sh"
 "Insert shebang sh")

(tempo-define-template
 "shebang-for-perl"
 '("#!/usr/bin/perl")
 "perl"
 "Insert shebang perl")

(tempo-define-template
 "RSpec feature define"
 '("RSpec.feature " "\"" (p "feature: " feature) "\" do" n
   ""p n
   "end" n)
 "feature"
 "Insert rspect feature blocks")

(tempo-define-template
 "form template"
 '("<%= form_with(model: "p
   ", local: true) do |form| %>" n
   ""n
   >"<%= form.submit %>"
   n>"<% end %>" n)
 "formwith"
 "Insert form_with blocks")

(tempo-define-template
 "bootstrap form template"
 '("<%= bootstrap_form_with(model: "p
   ", local: true, label_errors: true) do |form| %>"n
   ""n
   >"<%= form.primary %>"n
   "<% end %>"n)
 "bform"
 "Insert bootstrap_form_with blocks")

(defun try-tempo-complete-tag (old)
  (unless old
    (tempo-complete-tag)))

(add-to-list 'hippie-expand-try-functions-list 'try-tempo-complete-tag)

(provide 'init-tempo)
;;; init-tempo.el ends here
