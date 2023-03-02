;;; eglot-doc-posframe.el --- Insert description here -*- lexical-binding: t -*-
;;

;; URL: https://gist.github.com/llliilii/567dfadec3cd48c5e14e2335540b2937

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;; Commentary:

;; This package use adpated code from lsp-bridge
;; (https://github.com/manateelazycat/lsp-bridge), special thanks to its
;; contributors.

;; Use `eglot-doc-posframe-show' command to view eglot hove document in
;; posframe. You could scroll the document with C-v and M-v.

;; Configurations to disable hover doc in eldoc

;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-stay-out-of 'eldoc-documentation-functions)
;;   (add-to-list 'eglot-stay-out-of 'eldoc-documentation-strategy)

;; (defun my-eglot-setup-eldoc ()
;;     (cond ((eglot-managed-p)
;;            (add-hook 'eldoc-documentation-functions 'eglot-signature-eldoc-function nil t))
;;           (t
;;            (remove-hook 'eldoc-documentation-functions 'eglot-signature-eldoc-function t))))
;;   (add-hook 'eglot-managed-mode-hook #'my-eglot-setup-eldoc))

;;  `eglot--highlight-piggyback' was called only by `eglot-hover-eldoc-function'
;;  previously. So we should call it in `eglot-signature-eldoc-function' after
;;  we disable eglot hove doc in eldoc to enable textDocument/documentHighlight.
;; (define-advice eglot-signature-eldoc-function (:after (_cb) highlight)
;;     "Enable documentHighlight after remove `eglot-hover-eldoc-function'"
;;     (eglot--highlight-piggyback nil)
;;     t)

;;; Code:

(require 'eglot)
(require 'posframe)

(defgroup eglot-doc-posframe nil
  "Show eglot doc in posframe."
  :group 'eglot)

(defcustom edp-border-width 1
  "The border width of eglot doc posframe."
  :type 'integer
  :group 'eglot-doc-posframe)

(defface edp-border-color '((((background dark)) . (:background "white"))
                            (((background light)) . (:background "black")))
  "The border color used in childframe.")

(defcustom edp-max-width 150
  "The max width of eglot doc posframe."
  :type 'integer
  :group 'eglot-doc-posframe)

(defcustom edp-max-height 20
  "The max height of eglot doc posframe."
  :type 'integer
  :group 'eglot-doc-posframe)

(defface edp-background
  '((((background light)) :background "#f7e5a6")
    (t :background "#3d4566"))
  "Background color of the documentation."
  :group 'eglot-doc-posframe)


(defvar edp-name  "*eglot-doc-posframe*")

(defun edp--create ()
  "Create posframe `eglot-doc-posframe-name' and insert doc."
  (when (eglot--server-capable :hoverProvider)
    (let ((buf (current-buffer)))
      (jsonrpc-async-request
       (eglot--current-server-or-lose)
       :textDocument/hover (eglot--TextDocumentPositionParams)
       :success-fn (eglot--lambda ((Hover) contents range)
                     (eglot--when-buffer-window buf
                       (let ((info (unless (seq-empty-p contents)
                                     (eglot--hover-info contents range))))
                         (cond ((or (not info) (equal info ""))
                                (message "No EGLOT hover doc."))
                               ((posframe-workable-p)
                                (with-current-buffer (get-buffer-create edp-name)
                                  (erase-buffer)
                                  (insert (string-trim-right info)))
                                (posframe-show eglot-doc-posframe-name
                                               :position (point)
                                               :border-width edp-border-width
                                               :border-color (face-attribute
                                                              'edp-border-color :background)
                                               :background-color (face-attribute
                                                                  'edp-background :background)
                                               :max-width edp-max-width
                                               :max-height edp-max-height))
                               (t
                                (message "Posframe not workable!"))))))
       :deferred :textDocument/hover))))

(defun edp--hide ()
  "Hide posframe `eglot-doc-posframe-name'."
  (posframe-hide eglot-doc-posframe-name))


(defvar edp-active-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap scroll-up-command] #'edp-scroll-up)
    (define-key map [remap scroll-down-command] #'edp-scroll-down)
    map))

(define-minor-mode edp-active-mode
  "Enable when lsp-hover-doc is active."
  :keymap edp-active-map)

(defun edp-scroll-up ()
  "Scroll up eglot-doc-posframe buffer."
  (interactive)
  (posframe-funcall edp-name
                    #'call-interactively #'scroll-up-command))

(defun edp-scroll-down ()
  "Scroll down eglot-doc-posframe buffer."
  (interactive)
  (posframe-funcall edp-name
                    #'call-interactively #'scroll-down-command))

;;;###autoload
(defun eglot-doc-posframe-show ()
  "Display eglot-doc-posframe."
  (interactive)
  (edp--create)
  (edp-active-mode 1)
  (add-hook 'post-command-hook #'edp-monitor-post-command))

(defun edp-monitor-post-command ()
  "Auto hide hover posframe."
  (unless (memq this-command '(edp-scroll-up
                               edp-scroll-down
                               edp-show))
    (edp--hide)
    (edp-active-mode -1)
    (remove-hook 'post-command-hook #'edp-monitor-post-command)))

(provide 'eglot-doc-posframe)
;;; eglot-doc-posframe.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("edp-" . "eglot-doc-posframe-"))
;; End:
