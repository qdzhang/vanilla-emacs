;;; init-ml.el --- Config standard ml                -*- lexical-binding: t; -*-

;; Copyright (C) 2022  

;; Author:  <qdzhang@arch>
;; Keywords: 

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

(with-eval-after-load 'sml-mode
  (setq sml-program-name "smlnj"))

;; don't auto-close apostrophes (type 'a = foo) and backticks (`Foo)
(with-eval-after-load 'smartparens
  (sp-with-modes 'sml-mode
    (sp-local-pair "'" nil :actions nil)
    (sp-local-pair "`" nil :actions nil)
    (sp-local-pair "(* "  " *")))

(provide 'init-ml)
;;; init-ml.el ends here
