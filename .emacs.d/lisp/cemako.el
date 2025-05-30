;;; cemako.el --- Minimal CMake integration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 TideS

;; Author: TideS <tidesmain@gmail.com>
;; Ryan Pavlik <rpavlik@gmail.com>
;; Paul Nelson <ultrono@gmail.com>
;; Juan Jose Garcia-Ripoll <juanjose.garciaripoll@gmail.com>
;; URL: https://github.com/TideSofDarK/cemako.el
;; Keywords: convenience, languages
;; Package-Requires: ((emacs "30.1") (project "0.3.0"))
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

;; Minimal CMake integration for Emacs (that just works)
;;
;; Based on cmake-build.el by Ryan Pavlik
;;
;; This package stores configuration profiles and current state in .cemako.el
;; file at the project root (name and relative path may be customized).
;; That file is intended to be modified by hand but the package
;; also provides some convenient interactive prompts.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'project)
  (require 'compile)
  (require 'json))

(defgroup cemako ()
  "Use CMake to build projects and run targets based on configurations."
  :group 'tools)

(defcustom cemako-default-new-profile-name "Debug"
  "Default name for a new CMake profile."
  :type 'string
  :group 'cemako)

(defcustom cemako-default-new-profile-options "-DCMAKE_BUILD_TYPE=Debug"
  "Default options for a new CMake profile."
  :type 'string
  :group 'cemako)

(defcustom cemako-build-before-run t
  "Build automatically before running app, when using `cemako-run`."
  :type 'boolean
  :group 'cemako)

(defcustom cemako-build-dir-name-function 'cemako-default-build-dir-function
  "Specify a function to customize the build directory name.
By default, the name is in the form `build/<profile>`."
  :type 'function
  :group 'cemako)

(defcustom cemako-export-compile-commands 'copy
  "Either copy or symlink generated compile_commands.json to project root."
  :type 'symbol
  :group 'cemako
  :options '(copy symlink))

(defcustom cemako-project-root-function 'cemako-default-project-root-function
  "Return the project root."
  :type 'function
  :group 'cemako)

(defcustom cemako-project-name-function 'cemako-default-project-name-function
  "Return the project name."
  :type 'function
  :group 'cemako)

(defcustom cemako-run-function 'async-shell-command
  "Specify a function to use in `cemako-run'.
The function should accept as its first argument the shell
command to run (from `default-directory') and as its second
argument the name of the buffer to use."
  :type 'function
  :group 'cemako)

(defun cemako-default-project-root-function ()
  "Return the project root using `project.el'."
  (when-let* ((project (project-current)))
    (project-root project)))

(defun cemako--get-project-root ()
  "Return the project root"
  (funcall cemako-project-root-function))

(defun cemako-default-project-name-function ()
  "Return the project name."
  (when-let* ((project (project-current)))
    (project-name project)))

(defun cemako--project-data-path ()
  "Return path to project data file."
  (file-name-concat
   (file-name-as-directory (cemako--get-project-root))
   ".cemako.el"))

(defun cemako-edit-project-data ()
  "Open Cemako project file."
  (interactive)
  (find-file (cemako--project-data-path)))

(defun cemako--read-project-data ()
  "Read project data from .cemako.el file."
  (with-temp-buffer
    (let* ((project-data-path (cemako--project-data-path)))
      (or (when (file-exists-p project-data-path)
            (progn
              (insert-file-contents project-data-path)
              (if (string-blank-p (buffer-string))
                  (read "()")
                (car (read-from-string (buffer-string))))))
          (read "()")))))

(cl-defmacro cemako--with-project-data (&body body)
  "Execute BODY in the .cemako.el file buffer. Returns BODY."
  `(with-temp-buffer
     (let* ((project-data-path (cemako--project-data-path))
            (project-data
             (or (when (file-exists-p project-data-path)
                   (progn
                     (insert-file-contents project-data-path)
                     (if (string-blank-p (buffer-string))
                         (read "()")
                       (car (read-from-string (buffer-string))))))
                 (read "()"))))
       (prog1
           ,@body
         (erase-buffer)
         (cl-prettyprint project-data)
         (write-region 1 (point-max) project-data-path)))))

(cl-defmacro cemako--read-valid-project-data (tag &body body)
  "Execute BODY in the .cemako.el file buffer after validation.
Returns project data."
  `(cemako--with-project-data
    (when (cemako--validate project-data ,tag)
      (progn
        ,@body
        project-data))))

(defmacro cemako--add-to-alist (alist &rest elements)
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

(defun cemako--validity (project-data)
  "Return t if current project can be built or run.
Otherwise, return key explaining why not."
  (cond
   ((not (cemako--get-project-root))
    :data-missing)
   ((not (file-directory-p (cemako--get-build-dir project-data)))
    :build-dir-missing)
   (t t)))

(defun cemako--validate (project-data tag)
  "Return t if current project can be built or run.
Otherwise, return nil and print a message explaining why not.
TAG indicates the action being validated, e.g. `compile' or `run'."
  (not
   (cl-case (cemako--validity project-data)
     (:build-dir-missing
      (error "cemako %s: No build dir (%s)\nDo you need to initialize CMake?"
             tag
             (cemako--get-build-dir project-data)))
     (:data-missing
      (error "cemako %s: Not a valid project ;
no .cemako.el data found (project root is %s)"
	         tag
	         (cemako--get-project-root)))
     (t nil))))

(defun cemako-project-name ()
  "Return the project name.
Specified via the defcustom `cemako-project-name-function'."
  (let ((default-directory (cemako--get-project-root)))
    (funcall cemako-project-name-function)))

(defun cemako--get-options (project-data)
  "Return CMake options for current project."
  (let ((options (cdr (assoc 'options project-data))))
    (if (stringp options) options "")))

(defun cemako--get-profiles (project-data)
  "Return CMake profiles for current project."
  (cdr (assoc 'profiles project-data)))

(defun cemako--get-current-profile (project-data)
  "Return current CMake profile."
  (cdr (assoc 'current-profile project-data)))

(defun cemako--get-current-profile-options (project-data)
  "Return a string of arguments associated with current CMake profile."
  (when-let* ((current-profile (cemako--get-current-profile project-data))
              (profiles (cemako--get-profiles project-data)))
    (cdr (assoc current-profile profiles))))

(defun cemako--get-current-target (project-data)
  "Return current CMake target."
  (cdr (assoc 'current-target project-data)))

(defun cemako--get-current-target-executable (project-data)
  "Return current CMake target executable."
  (cdr (assoc 'current-target-executable project-data)))

(defun cemako-default-build-dir-function (project-data)
  "Return the default build directory, `build/PROFILE'."
  (concat "build/" (cemako--get-current-profile project-data)))

(defun cemako--get-build-dir (project-data)
  "Return the build directory for the current project."
  (concat
   (cemako--get-project-root)
   (funcall cemako-build-dir-name-function project-data)))

(defun cemako--check-build-dir ()
  "Check if the build directory exists."
  (let ((path (cemako--get-build-dir)))
    (if (file-directory-p path)
        t
      (message "Build directory doesn't exist: %s\n
Do you need to initialize CMake?" path)
      nil)))

(cl-defun cemako--compile (command &key sentinel)
  "Compile COMMAND with SENTINEL."
  (if (get-buffer-process "*compilation*")
      (message "Compilation is already running!")
    (let (compilation-buffer)
      (setq compilation-buffer (compile command))
      (when sentinel
        (let ((process (get-buffer-process compilation-buffer)))
          (when (process-live-p process)
            (set-process-sentinel process
                                  (lambda (p e)
                                    (compilation-sentinel p e)
                                    (funcall sentinel p e)))))))))

(defun cemako-edit-options (&optional options-string)
  "Set CMake options to OPTIONS-STRING."
  (interactive)
  (cemako--with-project-data
   (let* ((initial-options-string (cemako--get-options project-data))
          (options-string
           (or
            options-string
            (read-string "CMake options: " initial-options-string))))
     (when (stringp options-string)
       (cemako--add-to-alist project-data 'options options-string)))))

(defun cemako--completing-read-profile-name (project-data must-match)
  (interactive)
  (let* ((profiles (cemako--get-profiles project-data))
         (completion-extra-properties
          (list :annotation-function
                (lambda (profile)
                  (concat " " (cdr (assoc profile profiles))))))
         (choices (mapcar (lambda (x) (car x)) profiles)))
    (if (eq (length profiles) 0)
        (read-string "CMake profile name: " cemako-default-new-profile-name)
      (completing-read "CMake profile name: " choices nil must-match))))

(defmacro cemako--edit-profile (&optional profile-name profile-options)
  `(let* ((profiles (or (cdr (assoc 'profiles project-data)) '()))
          (current-profile-name (cemako--get-current-profile project-data))
          (profile-name
           (or ,profile-name
               (cemako--completing-read-profile-name project-data nil)))
          (current-profile-options
           (or (cemako--get-current-profile-options project-data)
               cemako-default-new-profile-options))
          (profile-options
           (or
            ,profile-options
            (read-string "CMake profile options: " current-profile-options))))
     (if (or (string-blank-p profile-name) (not (stringp profile-options)))
         (error "Incorrect profile name or profile options!")
       (progn
         (unless current-profile-name
           (cemako--add-to-alist project-data 'current-profile profile-name))
         (cemako--add-to-alist
          project-data
          'profiles
          (cemako--add-to-alist profiles
                                profile-name
                                profile-options))
         profile-name))))

(defun cemako-edit-profile (&optional profile-name profile-options)
  "Set CMake profile PROFILE-NAME with PROFILE-OPTIONS.
Returns edited profile name."
  (interactive)
  (cemako--with-project-data (cemako--edit-profile)))

(defun cemako-select-profile (&optional profile-name)
  "Set the current cmake profile to PROFILE-NAME."
  (interactive)
  (cemako--with-project-data
   (let* ((profiles (cemako--get-profiles project-data)))
     (cemako--add-to-alist
      project-data
      'current-profile
      (or profile-name
          (cemako--completing-read-profile-name project-data t))))))

(defun cemako--get-reply-files (build-dir)
  (let* ((reply-directory (expand-file-name ".cmake/api/v1/reply/" build-dir)))
    (and (file-exists-p reply-directory)
         (directory-files reply-directory 'full-name ".*\\.json"))))

(defun cemako--read-json (file)
  (with-temp-buffer
    (insert-file-contents file)
    (json-parse-buffer :object-type 'plist :array-type 'list)))

(defun cemako--target-type (target)
  (plist-get target :type))

(defun cemako--target-name (target)
  (plist-get target :name))

(defun cemako--target-artifact (target)
  "Return first artifact of a target."
  (plist-get (car (plist-get target :artifacts)) :path))

(defun cemako--get-targets (project-data
                            target-types
                            target-executables
                            &optional only-executables)
  (let* ((build-dir (cemako--get-build-dir project-data))
         (reply-files (cemako--get-reply-files build-dir))
         (target-names))
    (unless only-executables
      (push "all" target-names)
      (push "clean" target-names))
    (dolist (file reply-files)
      (let* ((json (cemako--read-json file))
             (target-name (cemako--target-name json))
             (target-type (cemako--target-type json))
             (is-executable (string= target-type "EXECUTABLE")))
        (when (and only-executables (not is-executable))
          (setq target-type nil))
        (unless (null target-type)
          (push target-name target-names)
          (puthash target-name target-type target-types)
          (when is-executable
            (puthash
             target-name
             (cemako--target-artifact json) target-executables)))))
    target-names))

(defmacro cemako--select-target (&optional target-name only-executables)
  `(let* ((target-types (make-hash-table :test 'equal))
          (target-executables (make-hash-table :test 'equal))
          (target-names
           (cemako--get-targets
            project-data target-types target-executables ,only-executables))
          (target-name
           (or ,target-name
               (let* ((completion-extra-properties
                       (list
                        :annotation-function
                        (lambda (target-name)
                          (concat " " (gethash target-name target-types))))))
                 (completing-read "Select CMake target: " target-names nil t)))))
     (cemako--add-to-alist
      project-data
      'current-target target-name)
     (cemako--add-to-alist
      project-data
      'current-target-executable (gethash target-name target-executables))
     target-name))

(defun cemako-select-target (&optional target-name only-executables)
  "Set the current CMake target to TARGET-NAME. Returns selected target name."
  (interactive)
  (cemako--with-project-data (cemako--select-target)))

(defun cemako--copy-compile-commands ()
  "Copy <build>/compile_commands.json to project root."
  (let ((filename
         (expand-file-name "compile_commands.json" (cemako--get-project-root))))
    (when (or (file-exists-p filename)
              (file-symlink-p filename))
      (delete-file filename)))
  (copy-file "compile_commands.json" (cemako--get-project-root)))

(defun cemako--symlink-compile-commands ()
  "Symlink <build>/compile_commands.json to project root."
  (let ((filename
         (expand-file-name "compile_commands.json" (cemako--get-project-root))))
    (when (or (file-exists-p filename)
              (file-symlink-p filename))
      (delete-file filename)))
  (make-symbolic-link
   (file-relative-name "compile_commands.json" (cemako--get-project-root))
   (cemako--get-project-root)))

(defun cemako--ensure-query-file-exists (build-dir)
  "Creates 'codemodel-v2' if not present."
  (let* ((query-file
          (expand-file-name ".cmake/api/v1/query/codemodel-v2" build-dir)))
    (when (not (file-exists-p query-file))
      (make-empty-file query-file t))))

(defun cemako-clear-cmake-cache ()
  "Clear CMake caches."
  (interactive)
  (when-let* ((project-data (cemako--read-project-data))
              (profile-name (cemako--get-current-profile project-data))
              (build-dir (cemako--get-build-dir project-data)))
    (when (file-exists-p build-dir)
      (let* ((default-directory build-dir))
        (dolist
            (cmake-cache (directory-files-recursively
                          "."
                          (rx (| "CMakeFiles" "CMakeCache.txt")) t))
          (when (file-directory-p cmake-cache)
            (message (concat "deleting: " cmake-cache))
            (delete-directory cmake-cache t t))
          (when (file-exists-p cmake-cache)
            (delete-file cmake-cache)))))))

(defun cemako-run-cmake ()
  "Run CMake with current profile."
  (interactive)
  (when-let* ((project-data
               (cemako--with-project-data
                (progn
                  (unless (cemako--get-current-profile project-data)
                    (cemako--edit-profile))
                  project-data)))
              (profile-name (cemako--get-current-profile project-data))
              (build-dir (cemako--get-build-dir project-data)))
    (unless (file-exists-p build-dir)
      (make-directory build-dir t))
    (let* ((default-directory build-dir)
           (command
            (concat "cmake -B . -S " (cemako--get-project-root)
                    " " (cemako--get-options project-data)
                    " " (cemako--get-current-profile-options project-data)
                    (when cemako-export-compile-commands
                      " -DCMAKE_EXPORT_COMPILE_COMMANDS=ON"))))
      (cemako--ensure-query-file-exists build-dir)
      (cemako--compile
       command
       :sentinel (lambda (p e)
                   (when (string-equal e "finished\n")
                     (cl-case cemako-export-compile-commands
                       (copy (cemako--copy-compile-commands))
                       (symlink (cemako--symlink-compile-commands)))))))))

(defun cemako-clean ()
  "Clean the current project."
  (interactive)
  (let* ((project-data (cemako--read-project-data))
         (default-directory (cemako--get-build-dir project-data)))
    (cemako--compile "cmake --build . --target clean")))

(defun cemako--invoke-build-current (project-data &optional sentinel)
  "Invoke build for current project.
If SENTINEL is non-nil, use it as the process sentinel."
  (when-let* ((target-name (cemako--get-current-target project-data))
              (default-directory (cemako--get-build-dir project-data))
              (command (concat "cmake --build .  --target " target-name)))
    (cemako--compile command :sentinel sentinel)))

(defun cemako-build ()
  "Build the current CMake target."
  (interactive)
  (when-let* ((project-data
               (cemako--read-valid-project-data
                "run"
                (unless (cemako--get-current-target project-data)
                  (cemako--select-target))))
              (current-target (cemako--get-current-target project-data)))
    (cemako--invoke-build-current project-data)))

(defun cemako--get-run-directory (config)
  "Get the run directory associated to CONFIG."
  (if (car config)
      (cemako--get-build-dir (car config))
    (cemako--maybe-remote-project-root)))

(defun cemako--invoke-run (project-data)
  "Invoke run for current project using CONFIG."
  (let* ((default-directory (cemako--get-build-dir project-data))
         (command
          (expand-file-name
           (cemako--get-current-target-executable project-data)
           default-directory)))
    (compile command)))

(defun cemako-run ()
  "Run the current target."
  (interactive)
  (when-let* ((project-data
               (cemako--read-valid-project-data
                "run"
                (unless (cemako--get-current-target-executable project-data)
                  (cemako--select-target nil t))))
              (default-directory (cemako--get-build-dir project-data))
              (current-target-executable
               (cemako--get-current-target-executable project-data)))
    (if cemako-build-before-run
        (cemako--invoke-build-current
         project-data
         (lambda (_process event)
           (when (cl-equalp "finished\n" event)
             (cemako--invoke-run project-data))))
      (cemako--invoke-run project-data))))

(defun cemako-debug ()
  "Run the current target in gdb."
  (interactive)
  (when-let* ((project-data
               (cemako--read-valid-project-data
                "run"
                (unless (cemako--get-current-target-executable project-data)
                  (cemako--select-target nil t))))
              (default-directory (cemako--get-build-dir project-data))
              (current-target-executable
               (cemako--get-current-target-executable project-data))
              (command
               (expand-file-name
                (cemako--get-current-target-executable project-data)
                default-directory)))
    (if cemako-build-before-run
        (cemako--invoke-build-current
         project-data
         (lambda (_process event)
           (when (cl-equalp "finished\n" event)
             (gdb (concat "gdb -i=mi --args " command)))))
      (gdb (concat "gdb -i=mi --args " command)))))

(provide 'cemako)
;;; cemako.el ends here
