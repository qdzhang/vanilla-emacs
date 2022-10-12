;;; init-fold.el --- Config emacs folding    -*- lexical-binding: t; -*-

;;; Commentary:

;; Combine `outline-minor-mode' and `hs-minor-mode':
;; - Use `outline-minor-mode' to separate buffer to into pieces.
;; - use `hs-minor-mode' to make marker, indent and syntax-based code folding.
;; Reference: the Doom Emacs `fold' module
;; https://github.com/doomemacs/doomemacs/tree/master/modules/editor/fold

;;; Code:

;; * Hideshow config
;; Don't hide comments when using `hs-hide-all', let `outline-minor-mode' do it
(setq hs-hide-comments-when-hiding-all nil)

;; ** mhtml hideshow config
;; https://stackoverflow.com/a/62502712
(defun mhtml-forward (arg)
  (interactive "P")
  (pcase (get-text-property (point) `mhtml-submode)
    (`nil (sgml-skip-tag-forward 1))
    (submode (forward-sexp))))

;; ** Extra folding support for more languages
;; From doom emacs: https://github.com/doomemacs/doomemacs/blob/7e50f239c46ea17429f159fb543c0d793543c06e/modules/editor/fold/config.el
(unless (assq 't hs-special-modes-alist)
  (setq hs-special-modes-alist
        (append
         '((vimrc-mode "{{{" "}}}" "\"")
           (yaml-mode "\\s-*\\_<\\(?:[^:]+\\)\\_>"
                      ""
                      "#"
                      +fold-hideshow-forward-block-by-indent-fn nil)
           (haml-mode "[#.%]" "\n" "/" +fold-hideshow-haml-forward-sexp-fn nil)
           (mhtml-mode "{\\|<[^/>]+?"
                       "}\\|</[^/>]*[^/]>"
                       "<!--"
                       mhtml-forward
                       nil)
           (matlab-mode "if\\|switch\\|case\\|otherwise\\|while\\|for\\|try\\|catch"
                        "end"
                        nil (lambda (_arg) (matlab-forward-sexp)))
           (nxml-mode "<!--\\|<[^/>]*[^/]>"
                      "-->\\|</[^/>]*[^/]>"
                      "<!--" sgml-skip-tag-forward nil)
           (latex-mode
            ;; LaTeX-find-matching-end needs to be inside the env
            ("\\\\begin{[a-zA-Z*]+}\\(\\)" 1)
            "\\\\end{[a-zA-Z*]+}"
            "%"
            (lambda (_arg)
              ;; Don't fold whole document, that's useless
              (unless (save-excursion
                        (search-backward "\\begin{document}"
                                         (line-beginning-position) t))
                (LaTeX-find-matching-end)))
            nil))
         hs-special-modes-alist
         '((t)))))

;; * Outline-minor-mode config

;; * Keybindings
(define-prefix-command 'my-fold-map nil "Fold-")
;; ** outline-minor-mode
;; HIDE
(define-key my-fold-map "q" 'outline-hide-sublevels)    ; Hide everything but the top-level headings
(define-key my-fold-map "t" 'outline-hide-body)         ; Hide everything but headings (all body lines)
(define-key my-fold-map "o" 'outline-hide-other)        ; Hide other branches
(define-key my-fold-map "c" 'outline-hide-entry)        ; Hide this entry's body
(define-key my-fold-map "l" 'outline-hide-leaves)       ; Hide body lines in this entry and sub-entries
(define-key my-fold-map "d" 'outline-hide-subtree)      ; Hide everything in this entry and sub-entries
;; SHOW
(define-key my-fold-map "a" 'outline-show-all)          ; Show (expand) everything
(define-key my-fold-map "e" 'outline-show-entry)        ; Show this heading's body
(define-key my-fold-map "i" 'outline-show-children)     ; Show this heading's immediate child sub-headings
(define-key my-fold-map "k" 'outline-show-branches)     ; Show all sub-headings under this heading
(define-key my-fold-map "s" 'outline-show-subtree)      ; Show (expand) everything in this heading & below
;; MOVE
(define-key my-fold-map "u" 'outline-up-heading)                ; Up
(define-key my-fold-map "n" 'outline-next-visible-heading)      ; Next
(define-key my-fold-map "p" 'outline-previous-visible-heading)  ; Previous
(define-key my-fold-map "f" 'outline-forward-same-level)        ; Forward - same level
(define-key my-fold-map "b" 'outline-backward-same-level)       ; Backward - same level

;; ** hs-minor-mode
(define-key my-fold-map "h" 'hs-hide-block)
(define-key my-fold-map "H" 'hs-show-block)
(define-key my-fold-map "g" 'hs-toggle-hiding)
(define-key my-fold-map "m" 'hs-hide-all)
(define-key my-fold-map "r" 'hs-show-all)

;; Global key
(global-set-key (kbd "C-c f") my-fold-map)

(provide 'init-fold)
;;; init-fold.el ends here
