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
   "---" n))

(provide 'init-tempo)
;;; init-tempo.el ends here
