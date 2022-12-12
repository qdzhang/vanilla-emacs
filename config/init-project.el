;;; init-project.el --- project.el config            -*- lexical-binding: t; -*-

(require 'project)

;; * Add new file types be determined as project root
;;
;; More example about add a new file to specify project root
;; https://www.reddit.com/r/emacs/comments/lfbyq5/specifying_projectroot_in_projectel/
;;
;; Some other extending of project.el
;; https://www.manueluberti.eu//emacs/2020/11/14/extending-project/
;;
;; I defined some separate file types as root-indicator formerly,
;; this is a  more generic method to detect the project root
;; https://andreyorst.gitlab.io/posts/2022-07-16-project-el-enhancements/
;; And there is a similar library:
;; https://github.com/buzztaiki/project-rootfile.el
(defcustom my/project-root-markers
  '(".project" "Gemfile" "go.mod" "Cargo.toml"
    "Makefile" "GNUMakefile" "CMakeLists.txt" "meson.build"
    "Cask" "Eldev" "Keg" "Eask"
    "Gruntfile.js" "gulpfile.js" "package.json"
    "project.clj"  "deps.edn" "shadow-cljs.edn"
    "dub.json" "dub.sdl")
  "Files or directories that indicate the root of a project."
  :type '(repeat string)
  :group 'project)

(defun my/project-root-p (path)
  "Check if the current PATH has any of the project root markers.

Use `dolist' to iterate my/project-root-markers."
  (catch 'found
    (dolist (marker my/project-root-markers)
      (when (file-exists-p (concat path marker))
        (throw 'found marker)))))

(defun my/project-root-p-1 (path)
  "Check if the current PATH has any of the project root markers.

Use `seq-some' to test at least one element of my/project-root-markers exists."
  (seq-some (lambda (f) (file-exists-p (expand-file-name f path)))
            my/project-root-markers))

(defun my/project-find-root (path)
  "Search up the PATH for `my/project-root-markers'."
  (when-let ((root (locate-dominating-file path #'my/project-root-p-1)))
    (cons 'transient (expand-file-name root))))

(add-to-list 'project-find-functions #'my/project-find-root)

;; Similar to `project-try-vc' but works when VC is disabled.
(defun my/project-try-magit (dir)
  "Similar to `project-try-vc' but works when VC is disabled.

URL: https://github.com/andschwa/.emacs.d/blob/main/init.el"
  (require 'magit-process)
  (let* ((root (magit-toplevel dir)))
    (and root (cons 'vc root))))

(add-to-list 'project-find-functions #'my/project-try-magit)

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
;; - Add the command `project-switch-to-buffer' when using `project-switch-project'
;; - Use `my/project-fd' to supersede `project-find-file'
(with-eval-after-load 'project
  (add-to-list 'project-switch-commands '(?b "Switch buffer" project-switch-to-buffer))
  (assq-delete-all 'project-find-file project-switch-commands)
  (assq-delete-all 'project-vc-dir project-switch-commands)
  (add-to-list 'project-switch-commands '(?v "Magit" magit-status))
  (add-to-list 'project-switch-commands '(?f "Fd-files" my/project-fd)))

;; * find files in a specific sub-directory
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
  "Create `.root' file in current directory."
  (interactive)
  (my/project-create-new-file ".root"))

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
    (my/ansi-term-bash)))

(defun my/project-remove-project ()
  "Remove project from `project--list' using completion.

URL: https://github.com/karthink/.emacs.d/blob/e0dd53000e61936a3e9061652e428044b9138c8c/lisp/setup-project.el#L78"
  (interactive)
  (project--ensure-read-project-list)
  (let* ((projects project--list)
         (dir (completing-read "REMOVE project from list: " projects nil t)))
    (setq project--list (delete (assoc dir projects) projects))))


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
    ("f" "Project find file" my/project-fd)
    ("F" "Project find regexp" project-find-regexp)
    ("d" "Project find dir" project-find-dir)
    ("R" "Project query and replece" project-query-replace-regexp)
    ("u" "Find sub-dir" my/project-find-file-in-dir)
    ("r" "Rg" rg-project)
    ("g" "Git files" my/project-git-find-files)]
   ["Switch"
    ("p" "Project switch project" project-switch-project)
    ("b" "Project switch buffer" project-switch-to-buffer)
    ("k" "Project kill buffers" project-kill-buffers)]]
  [["Actions"
    ("!" "Project shell command" project-shell-command)
    ("&" "Project async shell command" project-async-shell-command)
    ("c" "Project compile" project-compile)
    ("n" "Project new..." my-transient/project-new-menu)
    ("a" "Add .dir-locals.el" add-dir-local-variable)
    ("h" "Remove project" my/project-remove-project)]
   ["Modes"
    ("D" "Dired" project-dired)
    ("e" "Eshell" project-eshell)
    ("s" "Shell" project-shell)
    ("t" "Ansi-term" my/project-term)
    ("v" "VC dir" project-vc-dir)]])

(provide 'init-project)
