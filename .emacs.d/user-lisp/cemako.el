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

;; This package stores project configuration in `.cemako.el' file.
;; That file should be excluded from VC and is not meant to be edited by hand.

;;; TODO:

;; Hidden presets are not always hidden?
;; Make sure current preset is pre-selected when selecting.
;; Add platform checking.
;; Display an error on bad JSON.
;; Prevent creation of `.cemako.el' in arbitrary projects.
;; Prompt to select a target when the current one is not available anymore.

;;; Code:

(require 'cl-lib)
(require 'project)
(require 'compile)
(require 'json)

(defgroup cemako ()
  "Configure, build and run CMake projects using CMake presets feature."
  :group 'tools)

(defcustom cemako-build-before-run t
  "Build automatically before running app, when using `cemako-run'."
  :type 'boolean
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

(defcustom cemako-presets-json-template "{
  \"version\": 10,
  \"configurePresets\": [
    {
      \"name\": \"user-default\",
      \"displayName\": \"Default User Config\",
      \"description\": \"Default build using Ninja generator\",
      \"generator\": \"Ninja\",
      \"binaryDir\": \"${sourceDir}/build/default\"
    }
  ]
}"
  "For projects without presets JSON file, prompt to create one from
this template."
  :type 'string
  :group 'cemako)

(defun cemako-default-project-root-function ()
  "Return the project root using `project.el'."
  (when-let* ((project (project-current)))
    (project-root project)))

(defun cemako--get-project-root ()
  "Return the project root."
  (funcall cemako-project-root-function))

(defun cemako--project-data-path ()
  "Return path to project data file."
  (if-let* ((project-root (cemako--get-project-root)))
    (file-name-concat
      (file-name-as-directory project-root) ".cemako.el")
    (error "cemako.el needs active project!")))

(defconst cemako--empty-project-data '((version . 1)))

(defun cemako--read-project-data ()
  "Read project data from `.cemako.el' file."
  (if-let* ((project-data-path (cemako--project-data-path))
             (exists (file-exists-p project-data-path)))
    (with-temp-buffer
      (progn
        (insert-file-contents project-data-path)
        (if (string-blank-p (buffer-string))
          (map-copy cemako--empty-project-data)
          (car (read-from-string (buffer-string))))))
    (map-copy cemako--empty-project-data)))

(defun cemako--edit-project-data (project-data key value)
  "Set KEY to VALUE within PROJECT-DATA (destructively)."
  (let ((merged (map-merge 'list project-data `((,key . ,value)))))
    (setcar project-data (car merged))
    (setcdr project-data (cdr merged))))

(defun cemako--write-project-data (project-data)
  "Write PROJECT-DATA to `.cemako.el' file."
  (when-let* ((project-data-path (cemako--project-data-path)))
    (with-temp-buffer
      (erase-buffer)
      (cl-prettyprint project-data)
      (goto-char 0)
      (delete-char 1)
      (write-region (point-min) (point-max) project-data-path))))

(defmacro cemako--project-target ()
  "Return current CMake target."
  `(cdr (assoc 'target project-data)))

(defmacro cemako--project-target-executable ()
  "Return current CMake target executable."
  `(cdr (assoc 'target-executable project-data)))

(defmacro cemako--project-preset-name ()
  "Return current CMake preset name."
  `(cdr (assoc 'preset-name project-data)))

(defmacro cemako--project-binary-dir ()
  "Return current CMake binary directory."
  `(cdr (assoc 'binary-dir project-data)))

(defun cemako--get-reply-files (binary-dir)
  "Return a list of reply files (or nil if there are errors)."
  (when-let* ((reply-directory
                (expand-file-name ".cmake/api/v1/reply/" binary-dir))
               (reply-directory-exists (file-exists-p reply-directory))
               (reply-files
                 (directory-files reply-directory t ".*\\.json"))
               (no-errors
                 (not (any (lambda (reply-file)
                             (string-match-p
                               "^error"
                               (file-name-nondirectory reply-file)))
                        reply-files))))
    reply-files))

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

(defun cemako--get-targets (project-data)
  (let* ((binary-dir (cemako--project-binary-dir))
          (reply-files (cemako--get-reply-files binary-dir))
          (targets (make-hash-table :test 'equal)))
    (puthash "all" (list :type "SPECIAL") targets)
    (puthash "clean" (list :type "SPECIAL") targets)
    (dolist (file reply-files)
      (let* ((json (cemako--read-json file))
              (target-name (cemako--target-name json))
              (target-type (cemako--target-type json)))
        (unless (null target-type)
          (puthash
            target-name
            (list
              :type target-type
              :artifact (cemako--target-artifact json))
            targets))))
    targets))

(defun cemako--select-target (project-data
                               &optional targets target-name only-executables)
  (let* ((targets
           (or targets (cemako--get-targets project-data)))
          (target-names (if only-executables
                          (seq-filter
                            (lambda (target)
                              (string= "EXECUTABLE" (plist-get target :type)))
                            (hash-table-keys targets))
                          (hash-table-keys targets)))
          (target-name
            (or target-name
              (let* ((completion-extra-properties
                       (list
                         :annotation-function
                         (lambda (target-name)
                           (plist-get (gethash target-name targets) :type)))))
                (completing-read "Select CMake target: " target-names nil t))))
          (target (gethash target-name targets)))
    (cemako--edit-project-data
      project-data
      'target target-name)
    (cemako--edit-project-data
      project-data
      'target-executable (plist-get target :artifact))
    target-name))

(defun cemako--ensure-project-target (project-data)
  "Ensures a valid target is selected and returns its name."
  (let ((targets (cemako--get-targets project-data)))
    (if-let* ((selected-target (cemako--project-target))
               (contains (seq-contains-p
                           (hash-table-keys targets)
                           selected-target
                           #'string=)))
      selected-target
      (cemako--select-target project-data targets))))

(defconst cemako--presets-filenames '("CMakePresets.json"
                                       "CMakeUserPresets.json"))

;;; TODO: Add support for includes.
(defun cemako--collect-presets (filenames)
  "Parse CMake preset JSON files and return an alist of presets."
  (cl-loop for filename in filenames
    append (cl-loop for preset across (alist-get
                                        'configurePresets
                                        (json-read-file filename))
             collect (cons (alist-get 'name preset) preset))))

(defun cemako--selectable-presets (presets)
  "Filters out hidden presets and maps to `displayName'."
  (cl-loop
    for preset in (map-values presets)
    for hidden = (alist-get 'hidden preset)
    for preset-name = (alist-get 'name preset)
    for display-name = (alist-get 'displayName preset)
    for description = (alist-get 'description preset)
    if (or (not hidden) (eq hidden :json-false))
    collect `(,(or display-name preset-name)
               . ((name . ,preset-name)
                   (description . ,description)))))

;;; TODO: Merging is definitely not 100% correct.
;;; TODO: e. g. `cacheVariables' are not being merged properly.
(defun cemako--combine-preset (presets preset-name)
  "Recursively travels up the preset inheritance chain
and merges everything into single object."
  (when-let* ((preset (alist-get preset-name presets nil nil #'string=)))
    (let* ((inherits (alist-get 'inherits preset))
            (inherits-presets '())
            (index 0))
      ;; `inherits' could be either a string or a vector.
      (cond
        ((vectorp inherits)
          (while (< index (length inherits))
            (push
              (cemako--combine-preset presets (aref inherits index))
              inherits-presets)
            (setq index (1+ index))))
        ((stringp inherits)
          (push
            (cemako--combine-preset presets inherits)
            inherits-presets)))
      ;; Strip uninheritable properties.
      (let ((combined-parent (map-filter
                               (lambda (key _value)
                                 (not (or
                                        (eq key 'name)
                                        (eq key 'hidden)
                                        (eq key 'inherits)
                                        (eq key 'description)
                                        (eq key 'displayName))))
                               (apply 'map-merge 'list inherits-presets))))
        (map-merge 'list combined-parent preset)))))

(defun cemako--expand-preset-string (preset raw-string)
  "Replaces variety of macros in a preset string."
  (when (stringp raw-string)
    (setq raw-string (string-replace "${sourceDir}"
                       (cemako--get-project-root) raw-string))
    (setq raw-string (string-replace "${sourceParentDir}"
                       (file-name-parent-directory
                         (cemako--get-project-root))
                       raw-string))
    (setq raw-string (string-replace "${sourceDirName}"
                       (file-name-nondirectory
                         (directory-file-name
                           (file-name-directory
                             (cemako--get-project-root))))
                       raw-string))
    (setq raw-string (string-replace "${presetName}"
                       (alist-get 'name preset) raw-string))))

(defun cemako--get-preset-binary-dir (preset)
  "Return expanded binary directory for preset."
  (cemako--expand-preset-string preset (alist-get 'binaryDir preset)))

(defmacro cemako--cache-project-binary-dir (preset)
  "Takes expanded binary directory from a
preset (or prompts to enter it) and then caches it."
  `(when-let* ((project-binary-dir
                 (or (cemako--get-preset-binary-dir ,preset)
                   (read-directory-name "Output directory: "))))
     (cemako--edit-project-data
       project-data
       'binary-dir project-binary-dir)))

(defun cemako--select-preset (project-data presets)
  (when-let* ((default-directory (cemako--get-project-root))
               (selectable-presets (cemako--selectable-presets presets))
               (selectable-preset-name
                 (let* ((completion-extra-properties
                          (list
                            :annotation-function
                            (lambda (preset-name)
                              (alist-get
                                'description
                                (alist-get preset-name
                                  selectable-presets nil nil #'string=))))))
                   (completing-read
                     "Select CMake preset: " selectable-presets nil t)))
               (preset-name (alist-get
                              'name
                              (alist-get selectable-preset-name
                                selectable-presets nil nil #'string=)))
               (preset (cemako--combine-preset presets preset-name)))
    (cemako--edit-project-data
      project-data
      'preset-name
      preset-name)
    (cemako--cache-project-binary-dir preset)
    preset-name))

(defun cemako--ensure-preset-selected (project-data presets)
  "Ensures a preset is selected and returns its name."
  (if-let* ((binary-dir (cemako--project-binary-dir))
             (preset-name (cemako--project-preset-name))
             (preset (cl-find-if (lambda (el)
                                   (string= (alist-get 'name el) preset-name))
                       presets)))
    preset-name
    (cemako--select-preset project-data presets)))

(defun cemako--copy-compile-commands ()
  "Copy `compile_commands.json' to project root."
  (let ((project-root (cemako--get-project-root)))
    (let ((filename
            (expand-file-name "compile_commands.json" project-root)))
      (when (or (file-exists-p filename)
              (file-symlink-p filename))
        (delete-file filename)))
    (copy-file "compile_commands.json" project-root)))

(defun cemako--symlink-compile-commands ()
  "Symlink `compile_commands.json' to project root."
  (let ((project-root (cemako--get-project-root)))
    (let ((filename
            (expand-file-name "compile_commands.json" project-root)))
      (when (or (file-exists-p filename)
              (file-symlink-p filename))
        (delete-file filename)))
    (make-symbolic-link
      (file-relative-name "compile_commands.json" project-root)
      project-root)))

(defun cemako--compile (command &optional sentinel)
  "Compile COMMAND with SENTINEL process sentinel."
  (if (get-buffer-process "*compilation*")
    (message "Compilation is already running!")
    (let (compilation-buffer)
      (setq compilation-buffer (compile command))
      (when sentinel
        (let ((process (get-buffer-process compilation-buffer)))
          (when (process-live-p process)
            (set-process-sentinel
              process
              (lambda (p e)
                (compilation-sentinel p e)
                (funcall sentinel (and (eq (process-status p) 'exit)
                                    (zerop (process-exit-status p))))))))))))

(defun cemako--ensure-query-file-exists (binary-dir)
  "Creates `codemodel-v2' if not present."
  (let* ((query-file
           (expand-file-name ".cmake/api/v1/query/codemodel-v2" binary-dir)))
    (when (not (file-exists-p query-file))
      (make-empty-file query-file t))))

(defun cemako--configure (project-data preset-name &optional sentinel)
  (when-let* ((default-directory (cemako--get-project-root))
               (binary-dir (cemako--project-binary-dir))
               (command (concat "cmake --preset " preset-name
                          " -B " binary-dir
                          (when cemako-export-compile-commands
                            " -DCMAKE_EXPORT_COMPILE_COMMANDS=ON"))))
    (cemako--ensure-query-file-exists binary-dir)
    (cemako--compile
      command
      (lambda (success)
        (when success
          (let ((default-directory binary-dir))
            (cl-case cemako-export-compile-commands
              (copy (cemako--copy-compile-commands))
              (symlink (cemako--symlink-compile-commands)))))
        (when sentinel (funcall sentinel success))))))

(defun cemako--ensure-configured (project-data preset-name sentinel)
  (if-let* ((reply-files
              (cemako--get-reply-files (cemako--project-binary-dir))))
    (funcall sentinel t)
    (cemako--configure project-data preset-name sentinel)))

(defun cemako--presets-files-exist ()
  (any
    (lambda (el)
      (file-exists-p (expand-file-name el (cemako--get-project-root))))
    cemako--presets-filenames))

(defun cemako--existing-filenames (filenames)
  "Return a list of existing files within FILENAMES."
  (seq-filter
    (lambda (filename)
      (file-exists-p (expand-file-name filename (cemako--get-project-root))))
    filenames))

(defun cemako--ensure-presets ()
  (if-let* ((default-directory (cemako--get-project-root))
             (existing-presets-filenames
               (cemako--existing-filenames cemako--presets-filenames)))
    (if-let* ((presets (cemako--collect-presets existing-presets-filenames)))
      presets
      (error "No valid presets found!"))
    (progn
      (when (y-or-n-p "Can't find presets JSON files, \
create CMakeUserPresets.json?")
        (let ((filename (expand-file-name
                          "CMakeUserPresets.json"
                          (cemako--get-project-root))))
          (with-temp-file filename
            (erase-buffer)
            (insert cemako-presets-json-template))
          (find-file filename)))
      nil)))

(defun cemako--pipeline (project-data &optional on-built)
  "Executes complete CMake pipeline starting with
validating presets and ending with built binaries."
  (if-let* ((presets (cemako--ensure-presets))
             (preset-name (cemako--ensure-preset-selected project-data presets))
             (preset (cemako--combine-preset presets preset-name)))
    (cemako--ensure-configured
      project-data
      preset-name
      (lambda (success)
        (if-let* ((success success)
                   (target (cemako--ensure-project-target project-data)))
          (cemako--build
            project-data
            target
            (lambda (_success)
              (cemako--write-project-data project-data)
              (when on-built (funcall on-built))))
          (cemako--write-project-data project-data))))
    (cemako--write-project-data project-data)))

(defun cemako--build (project-data target &optional sentinel)
  "Build CMake target with name TARGET."
  (when-let* ((default-directory (cemako--project-binary-dir))
               (command (concat "cmake --build . --target " target)))
    (cemako--compile command sentinel)))

(defun cemako--run (project-data &optional build-first)
  (when-let* ((default-directory (cemako--project-binary-dir))
               (target (cemako--project-target))
               (target-executable (cemako--project-target-executable))
               (target-executable-path (expand-file-name
                                         target-executable
                                         default-directory))
               (target-executable-exists (file-exists-p
                                           target-executable-path)))
    (if build-first
      (cemako--build
        project-data
        target
        (lambda (success)
          (when success
            (compile target-executable-path))))
      (compile target-executable-path))))

;;;###autoload
(defun cemako-select-preset ()
  "Prompts to select a CMake preset."
  (interactive)
  (if-let* ((project-data (cemako--read-project-data))
             (presets (cemako--ensure-presets)))
    (when (cemako--select-preset project-data presets)
      (cemako--write-project-data project-data))))

;;;###autoload
(defun cemako-configure ()
  "Configure CMake project with current preset."
  (interactive)
  (if-let* ((project-data (cemako--read-project-data))
             (presets (cemako--ensure-presets))
             (preset-name (cemako--ensure-preset-selected project-data presets))
             (preset (cemako--combine-preset presets preset-name)))
    (cemako--configure project-data
      preset-name
      (lambda (_success)
        (cemako--write-project-data project-data)))
    (cemako--write-project-data project-data)))

;;;###autoload
(defun cemako-select-target ()
  "Prompts to select a CMake target."
  (interactive)
  (if-let* ((project-data (cemako--read-project-data))
             (presets (cemako--ensure-presets))
             (preset-name (cemako--ensure-preset-selected project-data presets))
             (preset (cemako--combine-preset presets preset-name)))
    (cemako--ensure-configured
      project-data
      preset-name
      (lambda (success)
        (when success
          (cemako--select-target project-data))
        (cemako--write-project-data project-data)))
    (cemako--write-project-data project-data)))

;;;###autoload
(defun cemako-build ()
  "Build selected CMake target."
  (interactive)
  (if-let* ((project-data (cemako--read-project-data))
             (binary-dir (cemako--project-binary-dir))
             (target (cemako--project-target)))
    (cemako--build project-data target)
    (cemako--pipeline project-data)))

;;;###autoload
(defun cemako-run ()
  "Run selected CMake target."
  (interactive)
  (let* ((project-data (cemako--read-project-data)))
    (unless (cemako--run project-data cemako-build-before-run)
      (cemako--pipeline
        project-data
        (lambda ()
          (cemako--run project-data))))))

;;;###autoload
(defun cemako-clean ()
  "Clean CMake project."
  (interactive)
  (when-let* ((project-data (cemako--read-project-data))
               (default-directory (cemako--project-binary-dir))
               (command (concat "cmake --build . --target clean")))
    (cemako--compile command)))

;;;###autoload
(defun cemako-clear-cmake-cache ()
  "Clear CMake caches."
  (interactive)
  (when-let* ((project-data (cemako--read-project-data))
               (binary-dir (cemako--project-binary-dir)))
    (when (file-exists-p binary-dir)
      (let* ((default-directory binary-dir))
        (dolist
          (cmake-cache (directory-files-recursively
                         "."
                         (rx (| "CMakeFiles" "CMakeCache.txt")) t))
          (when (file-directory-p cmake-cache)
            (message (concat "deleting: " cmake-cache))
            (delete-directory cmake-cache t t))
          (when (file-exists-p cmake-cache)
            (delete-file cmake-cache)))))))

(provide 'cemako)
;;; cemako.el ends here
