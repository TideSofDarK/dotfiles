;;; cmake-build.el --- CMake build profiles and configurations for projects  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020  Ryan Pavlik

;; Author: Ryan Pavlik <rpavlik@gmail.com>
;; URL: https://github.com/rpav/cmake-build.el
;; Package-Requires: ((emacs "29.1"))
;; Version: 1.0

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; (Fork by Paul Nelson.)

;;; Code:


(require 'cl-lib)
(require 'project)
(require 'compile)
(require 'json)

(defgroup cmake-build ()
  "Use CMake to build projects and run targets based on configurations."
  :group 'tools)

(defcustom cmake-build-run-window-autoswitch t
  "Automatically switch between Build and Run output buffers in the visible window."
  :type 'boolean
  :group 'cmake-build)

(defcustom cmake-build-before-run t
  "Build automatically before running app, when using `cmake-build-run`."
  :type 'boolean
  :group 'cmake-build)

(defcustom cmake-build-display-type 'split
  "How to display cmake-build output.
`split' will split the window (using cmake-build window
splitting options), `frame' will create a new frame.  In all
cases, the buffers will be reused if they are visible, regardless
of current display type."
  :type 'symbol
  :group 'cmake-build
  :options '(split frame))

(defcustom cmake-build-raise-frame t
  "Whether to raise the frame of the build window on build.
This only applies if `cmake-build-display-type` is frame."
  :type 'boolean
  :group 'cmake-build)

(defcustom cmake-build-run-window-size 20
  "Size of window to split."
  :type 'integer
  :group 'cmake-build)

(defcustom cmake-build-split-threshold 40.0
  "Threshold (percentage) at which to *not* split the current window.
Beyond this threshold, we instead use the other window.  That is,
if `cmake-build-run-window-size` is greater than this percentage
of the current window, it will not be split."
  :type 'float
  :group 'cmake-build)

(defcustom cmake-build-never-split nil
  "Never split the window, instead always use the other window."
  :type 'boolean
  :group 'cmake-build)

(defcustom cmake-build-switch-to-build nil
  "Whether to switch to build window with `cmake-build-current'."
  :type 'boolean
  :group 'cmake-build)

(defcustom cmake-build-dir-name-function 'cmake-build-default-build-dir-function
  "Specify a function to customize the build directory name.
By default, the name is in the form `build/<profile>`."
  :type 'function
  :group 'cmake-build)

(defcustom cmake-build-export-compile-commands 'copy
  "Either copy or symlink generated compile_commands.json to project root."
  :type 'symbol
  :group 'cmake-build
  :options '(copy symlink))

(defcustom cmake-build-project-root-function 'cmake-build-default-project-root-function
  "Return the project root."
  :type 'function
  :group 'cmake-build)

(defcustom cmake-build-project-name-function 'cmake-build-default-project-name-function
  "Return the project name."
  :type 'function
  :group 'cmake-build)

(defcustom cmake-build-run-function 'async-shell-command
  "Specify a function to use in `cmake-build-run'.
The function should accept as its first argument the shell
command to run (from `default-directory') and as its second
argument the name of the buffer to use."
  :type 'function
  :group 'cmake-build)

(defun cmake-build-default-project-root-function ()
  "Return the project root using `project.el'."
  (when-let ((project (project-current)))
    (project-root project)))

(defun cmake-build--get-project-root ()
  "Return the project root"
  (funcall cmake-build-project-root-function))

(defun cmake-build-default-project-name-function ()
  "Return the project name."
  (when-let ((project (project-current)))
    (project-name project)))

(cl-defmacro cmake-build--with-file ((filename &key readp writep) &body body)
  "Execute BODY in a temporary buffer using FILENAME.
If READP is non-nil, read the file into the buffer before BODY.
If WRITEP is non-nil, write the buffer to the file after BODY."
  `(with-temp-buffer
     (prog1
       ,(if readp
          `(when (file-exists-p ,filename)
             (insert-file-contents ,filename)
             ,@body)
          `(progn ,@body))
       (when ,writep
         (write-region 1 (point-max) ,filename)))))

(defun cmake-build--project-data-path ()
  "Return path to project data file."
  (file-name-concat
    (file-name-as-directory (cmake-build--get-project-root))
    ".cmake-build.el"))

(defun cmake-build-open-project-data ()
  "Open project data file."
  (interactive)
  (find-file (cmake-build--project-data-path)))

(defun cmake-build--read-project-data ()
  "Read project data from .cmake-build.el file."
  (let ((project-data-path (cmake-build--project-data-path)))
    (cmake-build--with-file (project-data-path :readp t)
      (read (buffer-string)))))

(cl-defmacro cmake-build--with-project-file ((&key readp writep) &body body)
  "Execute BODY in a temporary buffer using the local options file.
If READP is non-nil, read the file into the buffer before BODY.
If WRITEP is non-nil, write the buffer to the file after BODY."
  `(cmake-build--with-file ((cmake-build--project-data-path) :readp ,readp :writep ,writep) ,@body))

(defmacro cmake-build--add-to-alist (alist &rest elements)
  "Add the association of KEY and VAL to the value of ALIST.
If the list already contains an entry for KEY, update that entry;
otherwise prepend it to the list.

\(fn ALIST [KEY VAL]...)"
  `(progn
     ,@(cl-loop
         for (key val) on elements by #'cddr collect
         (if (< emacs-major-version 26)
           (macroexp-let2* nil ((k key) (p `(assoc ,k ,alist)))
             `(if ,p (setcdr ,p ,val)
                (push (cons ,k ,val) ,alist)))
           `(setf (alist-get ,key ,alist nil nil #'equal) ,val)))
     ,alist))

(defun cmake-build--validity (project-data)
  "Return t if current project can be built or run.
Otherwise, return key explaining why not."
  (cond
    ((not (cmake-build--get-project-root)) :data-missing)
    ((not (file-directory-p (cmake-build--get-build-dir project-data))) :build-dir-missing)
    (t t)))

(defun cmake-build--validate (project-data &optional tag)
  "Return t if current project can be built or run.
Otherwise, return nil and print a message explaining why not.
TAG indicates the action being validated, e.g. `compile' or `run'."
  (not
    (cl-case (cmake-build--validity project-data)
      (:build-dir-missing
        (message "cmake-build %s: No build dir (%s)\nDo you need to initialize CMake?"
          (or tag "compile")
          (cmake-build--get-build-dir project-data)))
      (:data-missing
        (message "cmake-build %s: Not a valid project ; no .cmake-build.el data found (project root is %s)"
	        (or tag "compile")
	        (cmake-build--get-project-root)))
      (t nil))))

(defun cmake-build-project-name ()
  "Return the project name.
Specified via the defcustom `cmake-build-project-name-function'."
  (let ((default-directory (cmake-build--get-project-root)))
    (funcall cmake-build-project-name-function)))

(defun cmake-build--build-buffer-name (project-data &optional _name)
  "Return the name of the build buffer."
  (concat "*Build " (cmake-build-project-name)
    "/" (cmake-build--get-current-profile project-data)
    ": " (cmake-build--get-current-target project-data)
    "*"))

(defun cmake-build--run-buffer-name (project-data)
  "Return the name of the run buffer."
  (concat "*Run " (cmake-build-project-name)
    "/" (cmake-build--get-current-profile project-data)
    ": " (cmake-build--get-current-target project-data)
    "*"))

(defun cmake-build--get-options (project-data)
  "Return CMake options for current project."
  (cdr (assoc 'options project-data)))

(defun cmake-build--get-profiles (project-data)
  "Return CMake profiles for current project."
  (cdr (assoc 'profiles project-data)))

(defun cmake-build--get-current-profile (project-data)
  "Return current CMake profile."
  (cdr (assoc 'current-profile project-data)))

(defun cmake-build--get-current-profile-options (project-data)
  "Return a string of arguments associated with current CMake profile."
  (when-let* ((current-profile (cdr (assoc 'current-profile project-data)))
               (profiles (assoc 'profiles project-data)))
    (cdr (assoc current-profile profiles))))

(defun cmake-build--get-current-target (project-data)
  "Return current CMake target."
  (cdr (assoc 'current-target project-data)))

(defun cmake-build--get-current-target-executable (project-data)
  "Return current CMake target executable."
  (cdr (assoc 'current-target-executable project-data)))

(defun cmake-build--get-configs ()
  "Return configs for current project."
  (when (cmake-build--get-project-root)
    (cdr (assoc 'cmake-build-run-configs
           (cmake-build--get-project-data)))))

(defun cmake-build--get-config (&optional config)
  "Return a specific CONFIG for current project."
  (cdr (assoc (or config (cmake-build-get-run-config-name))
         (cmake-build--get-configs))))

(defun cmake-build--get-build-config (&optional config)
  "Return build target associated to CONFIG."
  (let ((config (cmake-build--get-config config)))
    (cdr (assoc :build config))))

(defun cmake-build--get-run-config (&optional config)
  "Return run config associated to CONFIG."
  (let ((config (cmake-build--get-config config)))
    (cdr (assoc :run config))))

(defun cmake-build--get-run-config-env (&optional config)
  "Return run config environment associated to CONFIG."
  (let ((config (cmake-build--get-config config)))
    (cdr (assoc :env config))))

(defun cmake-build-default-build-dir-function (project-data)
  "Return the default build directory, `build/PROFILE'."
  (concat "build/" (cmake-build--get-current-profile project-data)))

(defun cmake-build--get-build-dir (project-data)
  "Return the build directory for the current project."
  (concat (cmake-build--get-project-root) (funcall cmake-build-dir-name-function project-data)))

(defun cmake-build--check-build-dir ()
  "Check if the build directory exists."
  (let ((path (cmake-build--get-build-dir)))
    (if (file-directory-p path)
      t
      (message "Build directory doesn't exist: %s\nDo you need to initialize CMake?" path)
      nil)))

(defun cmake-build--get-run-command (config)
  "Get the run command associated to CONFIG.
Return the concatenation of the second and third elements of
config, except when the first element is nil, in which case
prepend the build directory."
  (if (car config)
    (concat (cadr config) " " (caddr config))
    (concat
      (cmake-build--get-build-dir-relative)
      (concat (cadr config) " " (caddr config)))))

(defun cmake-build--switch-to-buffer (buffer buffer-window other-window)
  "Conditionally switch to BUFFER in OTHER-WINDOW.
Do this only if BUFFER-WINDOW is nil and
`cmake-build-run-window-autoswitch' is non-nil."
  (if buffer-window t
    (when (and cmake-build-run-window-autoswitch
            other-window)
      (set-window-dedicated-p other-window nil)
      (set-window-buffer other-window buffer)
      (set-window-dedicated-p other-window t)
      t)))

(defun cmake-build--split-to-buffer (name other-name)
  "Display buffers NAME and OTHER-NAME, adjusting window layout."
  (let* ((window-point-insertion-type t)
          ;; Make sure we have a buffer created regardless
          (buffer (get-buffer-create name))
          (current-buffer-window (get-buffer-window))
          (new-buffer-window (get-buffer-window name))
          (other-buffer-window (and other-name (get-buffer-window other-name t)))
          (split-is-current (or (eql current-buffer-window new-buffer-window)
                              (eql current-buffer-window other-buffer-window))))
    (when (or (and other-buffer-window
                cmake-build-run-window-autoswitch)
            (and (not cmake-build-never-split)
              (not split-is-current)
              (<= cmake-build-run-window-size
                (* (/ cmake-build-split-threshold 100.0)
                  (window-total-height current-buffer-window)))))
      (unless (cmake-build--switch-to-buffer buffer (get-buffer-window buffer t) other-buffer-window)
        (when (and (not other-buffer-window)
                (not (get-buffer-window name t)))
          (let ((window (split-window-below (- cmake-build-run-window-size))))
            (set-window-buffer window buffer)
            (set-window-dedicated-p window t))))
      t)))

(defun cmake-build--popup-buffer (name other-name)
  "Display buffers NAME and OTHER-NAME, possibly using pop-up frame."
  (let* ((buffer (get-buffer-create name))
          (_current-buffer-window (get-buffer-window buffer t))
          (other-buffer-window (and other-name (get-buffer-window other-name t)))
          (_buffer-config-name (cmake-build-get-run-config-name)))
    (unless (cmake-build--switch-to-buffer buffer (get-buffer-window buffer t) other-buffer-window)
      (display-buffer-pop-up-frame buffer default-frame-alist))
    (when cmake-build-raise-frame
      (raise-frame (window-frame (get-buffer-window buffer t))))
    t))

(defun cmake-build--display-buffer (name &optional other-name)
  "Display buffers NAME and OTHER-NAME.
The display method is determined by `cmake-build-display-type'."
  (cl-case cmake-build-display-type
    (split (cmake-build--split-to-buffer name other-name))
    (frame (cmake-build--popup-buffer name other-name))))

(cl-defun cmake-build--compile (buffer-name command &key sentinel other-buffer-name)
  "Compile COMMAND in BUFFER-NAME, with SENTINEL and OTHER-BUFFER-NAME."
  (let* ((did-split (cmake-build--display-buffer buffer-name other-buffer-name))
          (display-buffer-alist
            ;; Suppress the window only if we actually split
            (if did-split
              (cons (list buffer-name #'display-buffer-no-window)
                display-buffer-alist)
              display-buffer-alist))
          (actual-directory default-directory))
    (if (get-buffer-process buffer-name)
      (message "Already building %s"
        (funcall cmake-build-project-name-function))
      (with-current-buffer buffer-name
        (setq-local compilation-directory actual-directory)
        (setq-local default-directory actual-directory))
      ;; compile saves buffers; rely on this now
      (let (compilation-buffer)
        (cl-flet ((run-compile ()
                    (setq compilation-buffer (compile command))))
          (let ((w (get-buffer-window buffer-name t)))
            (if (and w (not (eql (get-buffer-window) w)))
              (if cmake-build-switch-to-build
                (progn
                  (switch-to-buffer-other-window buffer-name)
                  (run-compile))
                (with-selected-window w
                  (run-compile)))
              (run-compile))))
        (when sentinel
          (let ((process (get-buffer-process compilation-buffer)))
            (when (process-live-p process)
              (set-process-sentinel process
                (lambda (p e)
                  (funcall sentinel p e)
                  (compilation-sentinel p e))))))
        (with-current-buffer buffer-name
          (dolist (w (get-buffer-window-list buffer-name nil t))
            (set-window-point w (point-max)))
          (visual-line-mode 1))))))

(defun cmake-build--invoke-build-current (&optional sentinel)
  "Invoke build for current project.
If SENTINEL is non-nil, use it as the process sentinel."
  (let ((project-data (cmake-build--read-project-data)))
    (when (cmake-build--validate project-data)
      (let* ((default-directory (cmake-build--get-build-dir project-data))
              (command (concat "cmake --build .  --target " (cmake-build--get-current-target project-data)))
              (buffer-name (cmake-build--build-buffer-name project-data))
              (other-buffer-name (cmake-build--run-buffer-name project-data)))
        (cmake-build--compile buffer-name command
          :sentinel sentinel :other-buffer-name other-buffer-name)))))

(defun cmake-build-current ()
  "Build the current project."
  (interactive)
  (cmake-build--invoke-build-current))

(defun cmake-build--get-run-directory (config)
  "Get the run directory associated to CONFIG."
  (if (car config)
    (cmake-build--get-build-dir (car config))
    (cmake-build--maybe-remote-project-root)))

(defun cmake-build--invoke-run (project-data)
  "Invoke run for current project using CONFIG."
  (let* ((command (concat "./" (cmake-build--get-current-target-executable project-data)))
          (default-directory (cmake-build--get-build-dir project-data))
          ;; (process-environment (append
          ;;                       (list (concat "PROJECT_ROOT="
          ;;                                     (cmake-build--maybe-remote-project-root)))
          ;;                       (cmake-build--get-run-config-env)
          ;;                       process-environment))
          (buffer-name (cmake-build--run-buffer-name project-data))
          (other-buffer-name (cmake-build--build-buffer-name project-data))
          (display-buffer-alist
            (if (cmake-build--display-buffer buffer-name other-buffer-name)
              (cons (list buffer-name #'display-buffer-no-window)
                display-buffer-alist)
              display-buffer-alist)))
    (if (get-buffer-process buffer-name)
      (message "Already running %s"
        (funcall cmake-build-project-name-function))
      (message "Command: %s\n" command)
      (message "Directory: %s\n" default-directory)
      (message "Project root: %s\n" (cmake-build--get-project-root))
      (with-current-buffer buffer-name
        (insert command))
      (funcall cmake-build-run-function command buffer-name)
      ;; (with-current-buffer buffer-name
      ;;   (use-local-map cmake-build-run-keymap))
      )))

(defun cmake-build-run ()
  "Run the current target."
  (interactive)
  (let* ((project-data (cmake-build--read-project-data))
          (this-root (cmake-build--get-project-root)))
    (when (cmake-build--validate project-data "run")
      (if cmake-build-before-run
        (cmake-build--invoke-build-current
          (lambda (_process event)
            (let* ((this-root this-root))
              (when (cl-equalp "finished\n" event)
                (cmake-build--invoke-run project-data)))))
        (cmake-build--invoke-run project-data)))))

(defun cmake-build-debug ()
  "Run the current target in gdb."
  (interactive)
  (let* ((config (cmake-build--get-run-config))
          (command (cmake-build--get-run-command config))
          (default-directory (cmake-build--get-run-directory config)) ; check whether config should be (car config)
          (process-environment (append (cmake-build--get-run-config-env) process-environment)))
    (gdb (concat "gdb -i=mi --args " command))))

;; (defun cmake-build-set-options (option-string)
;;   "Set CMake build options to OPTION-STRING."
;;   (interactive
;;    (list
;;     (read-string "CMake build options: " cmake-build-options)))
;;   (setq cmake-build-options option-string))

(defun cmake-build-set-config (config-name)
  "Set the current config to CONFIG-NAME."
  (interactive
    (list
      (let* ((configs (cmake-build--get-configs))
              (choices (mapcar (lambda (x)
                                 (symbol-name (car x)))
                         configs)))
        (intern (funcall cmake-build-completing-read-function "CMake Config: " choices nil t nil nil (symbol-name (cmake-build-get-run-config-name)))))))
  (let* ((config (cmake-build--get-config config-name)))
    (if config
      (progn
        (cmake-build--set-run-config config-name)
        (let ((build (cmake-build--get-build-config))
               (run (cmake-build--get-run-config)))
          (message "Build: %s   Run: %s"
            (car build)
		        (if (car run)
			        (substring (cmake-build--get-run-command run)
                2)
		          (cmake-build--get-run-command run)))))
      (message "cmake-build: %s isn't a config." config))))

;; (defun cmake-build-set-buffer-local-config ()
;;   "Set current config, buffer-locally."
;;   (interactive)
;;   (setq-local cmake-build-run-config
;;               (list (copy-tree
;;                      (rassoc (cmake-build-get-run-config-name) cmake-build-run-config))))
;;   (call-interactively #'cmake-build-set-config))

(defun cmake-build-select-profile (&optional profile-name)
  "Set the current cmake profile to PROFILE-NAME."
  (interactive)
  (cmake-build--with-project-file (:readp t :writep t)
    (let* ((project-data (read (buffer-string)))
            (profiles (cmake-build--get-profiles project-data)))
      (cmake-build--add-to-alist
        project-data
        'current-profile
        (or profile-name
          (let* ((completion-extra-properties
                   (list :annotation-function (lambda (profile) (concat " " (cdr (assoc profile profiles))))))
                  (choices (mapcar (lambda (x) (car x)) profiles)))
            (completing-read "Select CMake profile: " choices nil t))))
      (erase-buffer)
      (cl-prettyprint project-data))))

(defun cmake-build--copy-compile-commands ()
  "Copy <build>/compile_commands.json to project root."
  (let ((filename (expand-file-name "compile_commands.json" (cmake-build--get-project-root))))
    (when (or (file-exists-p filename)
            (file-symlink-p filename))
      (delete-file filename)))
  (copy-file "compile_commands.json" (cmake-build--get-project-root)))

(defun cmake-build--symlink-compile-commands ()
  "Symlink <build>/compile_commands.json to project root."
  (let ((filename (expand-file-name "compile_commands.json" (cmake-build--get-project-root))))
    (when (or (file-exists-p filename)
            (file-symlink-p filename))
      (delete-file filename)))
  (make-symbolic-link (file-relative-name "compile_commands.json" (cmake-build--get-project-root))
    (cmake-build--get-project-root)))

(defun cmake-build--ensure-query-file-exists (build-dir)
  "Creates 'codemodel-v2' if not present."
  (let* ((query-file (expand-file-name ".cmake/api/v1/query/codemodel-v2" build-dir)))
    (when (not (file-exists-p query-file))
      (make-empty-file query-file t))))

(defun cmake-build-run-cmake ()
  "Clear caches and run CMake."
  (interactive)
  (let* ((project-data (cmake-build--read-project-data))
          (build-dir (cmake-build--get-build-dir project-data)))
    (unless (file-exists-p build-dir)
      (make-directory build-dir t))
    (let* ((default-directory build-dir)
            (buffer-name (cmake-build--build-buffer-name project-data))
            (other-buffer-name (cmake-build--run-buffer-name project-data))
            (command (concat "cmake -B . -S " (cmake-build--get-project-root)
                       " " (cmake-build--get-options project-data)
                       " " (cmake-build--get-current-profile-options project-data)
                       (when cmake-build-export-compile-commands
                         " -DCMAKE_EXPORT_COMPILE_COMMANDS=ON"))))
      (dolist (cmake-cache (directory-files-recursively "." (rx (| "CMakeFiles" "CMakeCache.txt")) t))
        (when (file-directory-p cmake-cache)
          (message (concat "deleting: " cmake-cache))
          (delete-directory cmake-cache t t))
        (when (file-exists-p cmake-cache)
          (delete-file cmake-cache)))
      (cmake-build--ensure-query-file-exists build-dir)
      (cmake-build--compile buffer-name command
        :sentinel (lambda (p e)
                    (when (string-equal e "finished\n")
                      (cl-case cmake-build-export-compile-commands
                        (copy (cmake-build--copy-compile-commands))
                        (symlink (cmake-build--symlink-compile-commands)))))
        :other-buffer-name other-buffer-name))))

(defun cmake-build-clean ()
  "Clean the current project."
  (interactive)
  (let* ((project-data (cmake-build--read-project-data))
          (default-directory (cmake-build--get-build-dir project-data))
          (buffer-name (cmake-build--build-buffer-name project-data))
          (other-buffer-name (cmake-build--run-buffer-name project-data)))
    (cmake-build--compile buffer-name "cmake --build . --target clean"
      :other-buffer-name other-buffer-name)))

(defun cmake-build--get-reply-files (build-dir)
  (let* ((reply-directory (expand-file-name ".cmake/api/v1/reply/" build-dir)))
    (and (file-exists-p reply-directory)
      (directory-files reply-directory 'full-name ".*\\.json"))))

(defun cmake-build--read-json (file)
  (with-temp-buffer
    (insert-file-contents file)
    (json-parse-buffer :object-type 'plist :array-type 'list)))

(defun cmake-build--target-type (target)
  (plist-get target :type))

(defun cmake-build--target-name (target)
  (plist-get target :name))

(defun cmake-build--target-artifact (target)
  "Return first artifact of a target."
  (plist-get (car (plist-get target :artifacts)) :path))

(defun cmake-build--get-available-targets (project-data target-types target-executables)
  (let* ((build-dir (cmake-build--get-build-dir project-data))
          (reply-files (cmake-build--get-reply-files build-dir))
          (target-names))
    (push "all" target-names)
    (push "clean" target-names)
    (dolist (file reply-files)
      (let* ((json (cmake-build--read-json file))
              (target-name (cmake-build--target-name json))
              (target-type (cmake-build--target-type json)))
        (unless (null target-type)
          (push target-name target-names)
          ;; (add-to-list target-name target-names)
          (puthash target-name target-type target-types)
          (when (string= target-type "EXECUTABLE")
            (puthash target-name (cmake-build--target-artifact json) target-executables)))))
    target-names))

(defun cmake-build-select-target (&optional target-name)
  "Set the current CMake target to TARGET-NAME."
  (interactive)
  (cmake-build--with-project-file (:readp t :writep t)
    (let* ((project-data (read (buffer-string)))
            (target-types (make-hash-table :test 'equal))
            (target-executables (make-hash-table :test 'equal))
            (target-names (cmake-build--get-available-targets project-data target-types target-executables))
            (target-name (or target-name
                           (let* ((completion-extra-properties
                                    (list :annotation-function (lambda (target-name) (concat " " (gethash target-name target-types))))))
                             (completing-read "Select CMake target: " target-names nil t)))))
      (cmake-build--add-to-alist project-data 'current-target target-name)
      (cmake-build--add-to-alist project-data 'current-target-executable (gethash target-name target-executables))
      (erase-buffer)
      (cl-prettyprint project-data))))

(provide 'cmake-build)
;;; cmake-build.el ends here
