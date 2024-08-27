;;; init-project.el --- project.el config            -*- lexical-binding: t; -*-

(require 'project)

;; References:
;; https://vannilla.org/write/1609258895/article.html
;; https://github.com/buzztaiki/project-rootfile.el
;; https://github.com/casouri/lunarymacs/blob/master/star/edit.el

(defun my/semantic-project-root (path)
  "Get PATH and return a string, (the root directory).
Used in `semanticdb-project-root-functions'."
  (let ((pr (project-current t)))
    (project-root pr)))

(setq project-vc-extra-root-markers '(".projectile.el" ".project.el" ".project"))

(add-to-list 'project-vc-ignores ".ccls-cache/")
(add-to-list 'project-vc-ignores "node_modules/")

;; Determine language-aware file as project root
(defvar project-language-aware-root-files
  '("Cargo.toml" "Gemfile" "go.mod"
    "compile_commands.json"
    "Cask" "Eldev" "Keg" "Eask"
    "project.clj"  "deps.edn" "shadow-cljs.edn"
    "dub.json" "dub.sdl"
    "compile_flags.txt"
    "tsconfig.json" "package.json"))

(defun my/project-try-language-aware (dir)
  "Find a super-directory of DIR containing a root file."
  (let ((dir (cl-loop for pattern in project-language-aware-root-files
                      for result = (locate-dominating-file dir pattern)
                      if result return result)))
    (and dir (cons 'language-aware dir))))

(cl-defmethod project-root ((project (head language-aware)))
  (cdr project))

(add-hook 'project-find-functions #'my/project-try-language-aware)

(defcustom project-common-ignores '("node_modules/" ".ccls-cache/")
  "List of patterns to add to `project-ignores'."
  :type '(repeat string))

(require 'grep)
(setq project-my-custom-ignores (append project-common-ignores grep-find-ignored-files))

(cl-defmethod project-ignores ((_project (head language-aware)) _dir)
  "Return the list of glob patterns to ignore."
  (append
   (cl-call-next-method)
   project-my-custom-ignores))


;; Determine explicit `.project' file as a project root
(defun my/project-try-explicit (dir)
  "Find a super-directory of DIR containing a root file."
  (let ((root (locate-dominating-file dir ".project")))
    (and root (cons 'explicit root))))

(cl-defmethod project-root ((project (head explicit)))
  (cdr project))

(add-hook 'project-find-functions #'my/project-try-explicit)

(cl-defmethod project-ignores ((_project (head explicit)) _dir)
  "Return the list of glob patterns to ignore."
  (append
   (cl-call-next-method)
   project-my-custom-ignores))

;; * Use fd to supersede default project find-file
(defun my/project-fd ()
  "Use `fd' to find files"
  (interactive)
  (let* ((pr (project-current t))
         (default-directory (project-root pr))
         (command (format "fd -H -t f --strip-cwd-prefix -0"))
         (cands (split-string (shell-command-to-string command) "\0" t))
         (file (completing-read "Fd file: " cands)))
    (when file
      (find-file file))))


;; * Customize `project-switch-commands'.
(setq project-switch-commands
      '((?f "Fd file" my/project-fd)
        (?g "Find regexp" project-find-regexp)
        (?d "Dired" project-dired)
        (?b "Buffer" project-switch-to-buffer)
        (?q "Query replace" project-query-replace-regexp)
        (?v "magit" my/project-magit-status)
        (?k "Kill buffers" project-kill-buffers)
        (?e "Eshell" project-eshell)))

(defun my/choose-directory (dir)
  "Choose a directory."
  (interactive "D") dir)

(defun my/project-find-file-in-dir ()
  "Find files in a specific directory. Use the default `project-find-file-in'.

URL: https://old.reddit.com/r/emacs/comments/tq552f/find_file_in_project_sub_directory/i2idk6j/"
  (interactive)
  (let* ((pr (project-current t))
         (default-directory (project-root pr))
         (dir (call-interactively 'my/choose-directory))
         (dirs (list dir)))
    (project-find-file-in (thing-at-point 'filename) dirs pr)))


(defun my/project-magit-status ()
  "Run magit-status in the current project's root."
  (interactive)
  (magit-status-setup-buffer (project-root (project-current t))))

(defun my/project-remove-project ()
  "Remove project from `project--list' using completion.

URL: https://github.com/karthink/.emacs.d/blob/e0dd53000e61936a3e9061652e428044b9138c8c/lisp/setup-project.el#L78"
  (interactive)
  (project--ensure-read-project-list)
  (let* ((projects project--list)
         (dir (completing-read "REMOVE project from list: " projects nil t)))
    (setq project--list (delete (assoc dir projects) projects))))

;; * Create new files in project root
(defun my/project-create-new-file (filename &optional file-content)
  "Create a new file named FILENAME with optional FILE-CONTENT."
  (let* ((proj (project-current))
         (proj-root (if proj (project-root proj)
                      default-directory))
         (filename-path (concat proj-root
                                (file-name-nondirectory filename))))
    (if (file-exists-p filename-path)
        (message "File %s exist." filename-path)
      (if file-content
          (with-temp-file filename-path
            (insert file-content))
        (with-temp-buffer
          (write-file filename-path)))
      (message "%s created." filename-path))))

(defun my/project-create-root-file ()
  "Create `.project' file in current directory."
  (interactive)
  (my/project-create-new-file ".project"))

(defun my/project-create-fdignore ()
  "Create `.fdignore' file in the root of current project."
  (interactive)
  (my/project-create-new-file ".fdignore" my/fdignore-content))

(defun my/project-create-jsconfig ()
  "Create `jsconfig.json' file in the root of current project."
  (interactive)
  (my/project-create-new-file "jsconfig.json" my/jsconfig-content))

(defun my/project-create-ccls ()
  "Create `.ccls' file in the root of current project root."
  (interactive)
  (my/project-create-new-file ".ccls" my/ccls-content))

(defun my/project-create-dir-locals ()
  "Create `.dir-locals.el' file in the root of current project."
  (interactive)
  (my/project-create-new-file ".dir-locals.el"))

(defun my/project-create-changelog ()
  "Create `ChangeLog' file in the root of current project."
  (interactive)
  (my/project-create-new-file "ChangeLog"))


(defvar my/fdignore-content
  "/node_modules\n\
/.git\n\
/.ccls-cache\n\
"
  "Content of .fdignore file.")

(defvar my/jsconfig-content
  "{\n\
  \"compilerOptions\": {\n\
    \"target\": \"es2017\",\n\
    \"allowSyntheticDefaultImports\": true,\n\
    \"noEmit\": true,\n\
    \"checkJs\": false,\n\
    \"jsx\": \"react\",\n\
    \"lib\": [\"dom\", \"es2017\"]\n\
  },\n\
  \"exclude\": [\"build\", \"node_modules\", \"assets/dependencies\"]\n\
}\n\
"
  "Content of jsconfig.json file.")

(defvar my/ccls-content
  "clang\n\
%c -std=c11\n\
%cpp -std=c++2a")


(defun my/project-git-find-files ()
  "Find file in the current Git repository."
  (interactive)
  (let* ((default-directory (locate-dominating-file
                             default-directory ".git"))
         (cands (split-string
                 (shell-command-to-string
                  "git ls-files --full-name --")))
         (file (completing-read "Find file: " cands)))
    (when file
      (find-file file))))

(defun my/project-term ()
  "Open `ansi-term' in current project root."
  (interactive)
  (let* ((pr (project-current t))
         (default-directory (project-root pr)))
    (ansi-term "/bin/bash")))


;; * Define transient menu
(transient-define-prefix my-transient/project-new-menu ()
  "Project new transient menu"
  ["Create"
   ("r" "Root file" my/project-create-root-file)
   ("f" "Fdignore" my/project-create-fdignore)
   ("j" "jsconfig" my/project-create-jsconfig)
   ("d" ".dir-locals" my/project-create-dir-locals)
   ("c" ".ccls" my/project-create-ccls)
   ("l" "changelog" my/project-create-changelog)])

(transient-define-prefix my-transient/project-menu ()
  "Porject transient menu invoked by prefix `C-x p'"
  [["Find"
    ("f" "Project fd file" my/project-fd)
    ("F" "Project find regexp" project-find-regexp)
    ("d" "Project find dir" project-find-dir)
    ("R" "Project query and replece" project-query-replace-regexp)
    ("u" "Find sub-dir" my/project-find-file-in-dir)
    ("r" "Rg" rg-project)
    ("o" "Original find file" project-find-file)
    ("g" "Git files" my/project-git-find-files)]
   ["Switch"
    ("p" "Project switch project" my/switch-project-in-new-tab)
    ("b" "Project switch buffer" project-switch-to-buffer)
    ("k" "Project kill buffers" project-kill-buffers)]]
  [["Actions"
    ("!" "Project shell command" project-shell-command)
    ("&" "Project async shell command" project-async-shell-command)
    ("c" "Project compile" project-compile)
    ("n" "Project new..." my-transient/project-new-menu)
    ("a" "Add .dir-locals.el" add-dir-local-variable)
    ("<deletechar>" "Remove project" my/project-remove-project)]
   ["Modes"
    ("D" "Dired" project-dired)
    ("e" "Eshell" project-eshell)
    ("s" "Shell" project-shell)
    ("t" "Ansi-term" my/project-term)
    ("v" "VC dir" project-vc-dir)]])

(provide 'init-project)
