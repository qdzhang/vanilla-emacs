;;; init-hi-lock.el --- Config hi-lock-mode          -*- lexical-binding: t; -*-

;;; Commentary:

;; 

;;; Code:

(defface hi-orange
  '((t :background "orange"))
  "Face to hi-lock with orange color"
  :group 'hi-lock-faces)

(defface hi-violet
  '((t :background "violet"))
  "Face to hi-lock with violet color"
  :group 'hi-lock-faces)

(defface hi-magenta
  '((t :background "magenta"))
  "Face to hi-lock with magenta color"
  :group 'hi-lock-faces)

(defface hi-peru
  '((t :background "peru"))
  "Face to hi-lock with peru color"
  :group 'hi-lock-faces)

(defface hi-sky-blue
  '((t :background "sky blue"))
  "Face to hi-lock with sky blue color"
  :group 'hi-lock-faces)

(defface hi-bisque
  '((t :background "bisque"))
  "Face to hi-lock with bisque color"
  :group 'hi-lock-faces)

(defface hi-lavender
  '((t :background "lavender"))
  "Face to hi-lock with lavender color"
  :group 'hi-lock-faces)

(defface hi-PaleGreen2
  '((t :background "PaleGreen2"))
  "Face to hi-lock with PaleGreen2 color"
  :group 'hi-lock-faces)

(defface hi-CadetBlue3
  '((t :background "CadetBlue3"))
  "Face to hi-lock with CadetBlue3 color"
  :group 'hi-lock-faces)

(setq hi-lock-face-defaults
      '("hi-yellow" "hi-pink" "hi-green" "hi-blue" "hi-salmon" "hi-aquamarine"
        "hi-orange" "hi-violet" "hi-magenta" "hi-peru" "hi-sky-blue"
        "hi-bisque" "hi-lavender" "hi-PaleGreen2" "hi-CadetBlue3"))


(provide 'init-hi-lock)
;;; init-hi-lock.el ends here
