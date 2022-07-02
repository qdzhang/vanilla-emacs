;;; init-docview.el --- Configure doc-view-mode      -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(with-eval-after-load "doc-view"
  (setq doc-view-cache-directory (expand-file-name "~/Documents/emacs-doc-view-cache/"))
  (setq doc-view-resolution 300)
  (setq doc-view-continuous t)

  (easy-menu-define my-doc-view-menu doc-view-mode-map "Menu for Doc-View Mode."
    '("DocView"
      ["Switch to a different mode" doc-view-toggle-display :help "Switch to a different mode"]
      ["Open Text" doc-view-open-text :help "Display the current doc's contents as text"]
      "--"
      ("Navigate Doc"
       ["Goto Page ..." doc-view-goto-page :help "View the page given by PAGE"]
       "--"
       ["Scroll Down" doc-view-scroll-down-or-previous-page :help "Scroll page down ARG lines if possible, else goto previous page"]
       ["Scroll Up" doc-view-scroll-up-or-next-page :help "Scroll page up ARG lines if possible, else goto next page"]
       "--"
       ["Next Line" doc-view-next-line-or-next-page :help "Scroll upward by ARG lines if possible, else goto next page"]
       ["Previous Line" doc-view-previous-line-or-previous-page :help "Scroll downward by ARG lines if possible, else goto previous page"]
       ("Customize"
        ["Continuous Off"
         (setq doc-view-continuous nil)
         :help "Stay put in the current page, when moving past first/last line" :style radio :selected
         (eq doc-view-continuous nil)]
        ["Continuous On"
         (setq doc-view-continuous t)
         :help "Goto to the previous/next page, when moving past first/last line" :style radio :selected
         (eq doc-view-continuous t)]
        "---"
        ["Save as Default"
         (customize-save-variable 'doc-view-continuous doc-view-continuous)
         t])
       "--"
       ["Next Page" doc-view-next-page :help "Browse ARG pages forward"]
       ["Previous Page" doc-view-previous-page :help "Browse ARG pages backward"]
       "--"
       ["First Page" doc-view-first-page :help "View the first page"]
       ["Last Page" doc-view-last-page :help "View the last page"])
      "--"
      ("Adjust Display"
       ["Enlarge" doc-view-enlarge :help "Enlarge the document by FACTOR"]
       ["Shrink" doc-view-shrink :help "Shrink the document"]
       "--"
       ["Fit Width To Window" doc-view-fit-width-to-window :help "Fit the image width to the window width"]
       ["Fit Height To Window" doc-view-fit-height-to-window :help "Fit the image height to the window height"]
       "--"
       ["Fit Page To Window" doc-view-fit-page-to-window :help "Fit the image to the window"]
       "--"
       ["Set Slice From Bounding Box" doc-view-set-slice-from-bounding-box :help "Set the slice from the document's BoundingBox information"]
       ["Set Slice Using Mouse" doc-view-set-slice-using-mouse :help "Set the slice of the images that should be displayed"]
       ["Set Slice" doc-view-set-slice :help "Set the slice of the images that should be displayed"]
       ["Reset Slice" doc-view-reset-slice :help "Reset the current slice"])
      ("Search"
       ["New Search ..."
        (doc-view-search t)
        :help "Jump to the next match or initiate a new search if NEW-QUERY is given"]
       "--"
       ["Search" doc-view-search :help "Jump to the next match or initiate a new search if NEW-QUERY is given"]
       ["Backward" doc-view-search-backward :help "Call `doc-view-search' for backward search"]
       "--"
       ["Show Tooltip" doc-view-show-tooltip :help nil])
      ("Maintain"
       ["Reconvert Doc" doc-view-reconvert-doc :help "Reconvert the current document"]
       "--"
       ["Clear Cache" doc-view-clear-cache :help "Delete the whole cache (`doc-view-cache-directory')"]
       ["Dired Cache" doc-view-dired-cache :help "Open `dired' in `doc-view-cache-directory'"]
       "--"
       ["Revert Buffer" doc-view-revert-buffer :help "Like `revert-buffer', but preserves the buffer's current modes"]
       "--"
       ["Kill Proc" doc-view-kill-proc :help "Kill the current converter process(es)"]
       ["Kill Proc And Buffer" doc-view-kill-proc-and-buffer :help "Kill the current buffer"])
      "--"
      ["Customize"
       (customize-group 'doc-view)]))
  (easy-menu-define my-doc-view-minor-mode-menu doc-view-minor-mode-map "Menu for Doc-View Minor Mode."
    '("DocView*"
      ["Display in DocView Mode" doc-view-toggle-display :help "View"]
      ["Exit DocView Mode" doc-view-minor-mode])))


(provide 'init-docview)
;;; init-docview.el ends here
