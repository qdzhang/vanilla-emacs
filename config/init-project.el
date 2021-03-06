;;; init-project.el --- project.el config            -*- lexical-binding: t; -*-

;; More example about add a new file to specify project root
;; https://www.reddit.com/r/emacs/comments/lfbyq5/specifying_projectroot_in_projectel/

;; Declare directories with "go.mod" as a project
(cl-defmethod project-root ((project (head go-module)))
  (cdr project))

(defun my/project-find-go-module (dir)
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

;; Some other extending of project.el
;; https://www.manueluberti.eu//emacs/2020/11/14/extending-project/

;; Declare directories with ".project" as a project
(cl-defmethod project-root ((project (head local)))
  (cdr project))

(defun my/project-try-local (dir)
  "Determine if DIR is a non-Git project.
DIR must include a .project file to be considered a project."
  (let ((root (locate-dominating-file dir ".project")))
    (and root (cons 'local root))))

(add-hook 'project-find-functions #'my/project-find-go-module)
(add-hook 'project-find-functions #'my/project-try-local)

(defun my--project-files-in-directory (dir)
  "Use `fd' to list files in DIR."
  (let* ((default-directory dir)
         (localdir (file-local-name (expand-file-name dir)))
         (command (format "fd -H -t f -0 . %s" localdir)))
    (project--remote-file-names
     (sort (split-string (shell-command-to-string command) "\0" t)
           #'string<))))

(cl-defmethod project-files ((project (head local)) &optional dirs)
  "Override `project-files' to use `fd' in local projects."
  (mapcan #'my--project-files-in-directory
          (or dirs (list (project-root project)))))

;; Add the command `project-switch-to-buffer' when using `project-switch-project'
(with-eval-after-load 'project
  (add-to-list 'project-switch-commands '(?b "Switch buffer" project-switch-to-buffer)))

(defun my/create-project-root-file ()
  "Create .project file at project root."
  (interactive)
  (let ((projectroot (cdr (project-current))))
    (if projectroot
        (let ((projectroot-file (concat projectroot ".project")))
          (if (file-exists-p projectroot-file)
              (message "Project root file exists")
            (with-temp-buffer (write-file projectroot-file))))
      (let ((projectroot-file (concat default-directory ".project")))
        (with-temp-buffer (write-file projectroot-file))
        (message ".project file created")))))

(defvar fdignore-content
  "/node_modules\n\
/.git\n\
/.ccls-cache\n\
"
  "Content of .fdignore file.")

(defun my/create-fd-ignore-file ()
  "Create a fdignore file at project root."
  (interactive)
  (let ((fdignore (cdr (project-current))))
    (if fdignore
        (let ((fdignore-file (concat fdignore ".fdignore")))
          (if (file-exists-p fdignore-file)
              (message "File exists")
            (with-temp-file fdignore-file
              (insert fdignore-content))))
      (message ".fdignore file created"))))

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

(defun my/create-jsconfig-file ()
  "Create a jsconfig file at project root."
  (interactive)
  (let ((jsconfig (cdr (project-current))))
    (if jsconfig
        (let ((jsconfig-file (concat jsconfig "jsconfig.json")))
          (if (file-exists-p jsconfig-file)
              (message "File exists")
            (with-temp-file jsconfig-file
              (insert my/jsconfig-content))))
      (message "Project not found"))))

(transient-define-prefix my-transient/project-new-menu ()
  "Project new transient menu"
  ["Create"
   ("r" "Root file" my/create-project-root-file)
   ("f" "Fdignore" my/create-fd-ignore-file)
   ("j" "jsconfig" my/create-jsconfig-file)])

(transient-define-prefix my-transient/project-menu ()
  "Porject transient menu invoked by prefix `C-x p'"
  [["Find"
    ("f" "Project find file" project-find-file)
    ("g" "Project find regexp" project-find-regexp)
    ("d" "Project find dir" project-find-dir)
    ("r" "Project query and replece" project-query-replace-regexp)]
   ["Switch"
    ("p" "Project switch project" project-switch-project)
    ("b" "Project switch buffer" project-switch-to-buffer)
    ("k" "Project kill buffers" project-kill-buffers)]]
  [["Actions"
    ("!" "Project shell command" project-shell-command)
    ("&" "Project async shell command" project-async-shell-command)
    ("c" "Project compile" project-compile)
    ("n" "Peoject new..." my-transient/project-new-menu)]
   ["Modes"
    ("D" "Dired" project-dired)
    ("e" "Eshell" project-eshell)
    ("s" "Shell" project-shell)
    ("v" "VC dir" project-vc-dir)]])

(provide 'init-project)
