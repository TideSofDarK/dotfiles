;; Disable GC During Init

(put 'gc-cons-percentage 'original-value-before-init gc-cons-percentage)
(put 'gc-cons-percentage 'value-during-init 0.6)
(defun restore-gc-cons-percentage-after-init ()
    (let ((expected-value (get 'gc-cons-percentage 'value-during-init))
             (value-to-restore (get 'gc-cons-percentage 'original-value-before-init)))
        (when (and value-to-restore (equal gc-cons-percentage expected-value))
            (setq gc-cons-percentage value-to-restore))))
(add-hook 'after-init-hook #'restore-gc-cons-percentage-after-init)
(setq gc-cons-percentage (get 'gc-cons-percentage 'value-during-init))

;; Maximize On Launch

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Add Lisp Path

(add-to-list 'load-path (concat user-emacs-directory "lisp"))

;; elpaca

(setq package-enable-at-startup nil)
(defvar elpaca-installer-version 0.10)
(defvar elpaca-directory
    (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory
    (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory
    (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order
    '(elpaca
         :repo "https://github.com/progfolio/elpaca.git"
         :ref nil
         :depth 1
         :inherit ignore
         :files (:defaults "elpaca-test.el" (:exclude "extensions"))
         :build (:not elpaca--activate-package)))
(let* ((repo (expand-file-name "elpaca/" elpaca-repos-directory))
          (build (expand-file-name "elpaca/" elpaca-builds-directory))
          (order (cdr elpaca-order))
          (default-directory repo))
    (add-to-list
        'load-path
        (if (file-exists-p build)
            build
            repo))
    (unless (file-exists-p repo)
        (make-directory repo t)
        (when (<= emacs-major-version 28)
            (require 'subr-x))
        (condition-case-unless-debug err
            (if-let* ((buffer
                          (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                         ((zerop
                              (apply #'call-process
                                  `("git" nil ,buffer t "clone" ,@
                                       (when-let* ((depth
                                                       (plist-get order :depth)))
                                           (list
                                               (format "--depth=%d" depth)
                                               "--no-single-branch"))
                                       ,(plist-get order :repo) ,repo))))
                         ((zerop
                              (call-process "git"
                                  nil
                                  buffer
                                  t
                                  "checkout"
                                  (or (plist-get order :ref) "--"))))
                         (emacs
                             (concat invocation-directory invocation-name))
                         ((zerop
                              (call-process
                                  emacs
                                  nil
                                  buffer
                                  nil
                                  "-Q"
                                  "-L"
                                  "."
                                  "--batch"
                                  "--eval"
                                  "(byte-recompile-directory \".\" 0 'force)")))
                         ((require 'elpaca))
                         ((elpaca-generate-autoloads "elpaca" repo)))
                (progn
                    (message "%s" (buffer-string))
                    (kill-buffer buffer))
                (error
                    "%s"
                    (with-current-buffer buffer
                        (buffer-string))))
            ((error) (warn "%s" err) (delete-directory repo 'recursive))))
    (unless (require 'elpaca-autoloads nil t)
        (require 'elpaca)
        (elpaca-generate-autoloads "elpaca" repo)
        (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))
(elpaca elpaca-use-package
    (elpaca-use-package-mode)
    (setq elpaca-use-package-by-default t))
(elpaca-wait)

;; Fonts

(let ((mono-spaced-font "Sarasa Term Slab CL")
         (proportionately-spaced-font "Sarasa UI CL"))
    (set-face-attribute 'default nil
        :family mono-spaced-font
        :height 120)
    (set-face-attribute 'fixed-pitch nil
        :family mono-spaced-font
        :height 1.0)
    (set-face-attribute 'variable-pitch nil
        :family proportionately-spaced-font
        :height 1.0))

;; Theme

(setq kanagawa-themes-keyword-italic t)
(setq kanagawa-themes-comment-italic t)
(require 'kanagawa-themes)
(load-theme 'kanagawa-wave :no-confirm-loading)

;; minions

(use-package minions
    :ensure t
    :config (minions-mode 1))

;; emacs

(use-package
    emacs
    :ensure nil
    :custom
    (frame-title-format '("GNU Emacs"))
    ;; (icon-title-format '("GNU Emacs"))
    (fringe-mode 0)
    (savehist-mode 1)
    (delete-by-moving-to-trash t)
    (use-short-answers t)
    (frame-inhibit-implied-resize t)
    ;; (frame-resize-pixelwise t)
    (auto-mode-case-fold nil)
    (warning-suppress-log-types '((native-compiler)))
    (ring-bell-function 'ignore)
    (help-window-select t)
    (tab-always-indent t)
    (menu-bar-mode nil)
    (scroll-bar-mode nil)
    (tool-bar-mode nil)
    (tooltip-mode -1)
    (inhibit-startup-screen t)
    (inhibit-splash-screen t)
    (initial-scratch-message nil)
    (delete-selection-mode t)
    (electric-indent-mode t)
    (electric-pair-mode t)
    (blink-cursor-mode nil)
    (revert-without-query (list "."))
    (auto-revert-stop-on-user-input nil)
    (auto-revert-verbose t)
    (global-auto-revert-non-file-buffers t)
    (global-auto-revert-ignore-modes '(Buffer-menu-mode))
    (global-auto-revert-mode t)
    (recentf-mode t)
    (truncate-lines t)
    (text-mode-ispell-word-completion nil)
    (read-extended-command-predicate
        #'command-completion-default-include-p)
    (display-line-numbers-type 'relative)
    (display-line-numbers-grow-only t)
    (display-line-numbers-width 3)
    (display-line-numbers-width-start t)
    (global-display-line-numbers-mode t)
    (mouse-wheel-progressive-speed nil)
    (scroll-error-top-bottom t)
    (scroll-margin 6)
    (scroll-conservatively 101)
    (scroll-up-aggressively 0.01)
    (scroll-down-aggressively 0.01)
    (scroll-preserve-screen-position t)
    (auto-window-vscroll nil)
    (indent-tabs-mode nil)
    (tab-width 4)
    (use-dialog-box nil)
    (use-file-dialog nil)
    (confirm-kill-processes nil)
    (confirm-kill-emacs nil)
    (set-buffer-modified-p nil)
    (create-lockfiles nil)
    (make-backup-files nil)
    (auto-save-default nil)
    (enable-recursive-minibuffers t)
    (minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
    (eldoc-echo-area-use-multiline-p nil)
    (switch-to-buffer-obey-display-actions t)
    ;; (pixel-scroll-precision-mode t)
    ;; (pixel-scroll-precision-use-momentum nil)
    ;; (global-hl-line-mode 1)
    (bidi-display-reordering 'left-to-right)
    (bidi-paragraph-direction 'left-to-right)
    (bidi-inhibit-bpa t)
    (initial-major-mode 'fundamental-mode)
    (redisplay-skip-fontification-on-input t)
    (fast-but-imprecise-scrolling t)
    (window-divider-default-bottom-width 1)
    (window-divider-default-places t)
    (window-divider-default-right-width 1)
    (mouse-yank-at-point nil)
    (indicate-buffer-boundaries nil)
    (indicate-empty-lines nil)
    :config
    ;; (defun skip-these-buffers (_window buffer _bury-or-kill)
    ;;     "Function for `switch-to-prev-buffer-skip'."
    ;;     (string-match "\\*[^*]+\\*" (buffer-name buffer)))
    ;; (setq switch-to-prev-buffer-skip 'skip-these-buffers)
    (modify-coding-system-alist 'file "" 'utf-8)
    (advice-add 'display-startup-echo-area-message :override #'ignore)
    (advice-add 'display-startup-screen :override #'ignore)
    (setq custom-file (locate-user-emacs-file "custom.el"))
    (load custom-file 'noerror 'nomessage)
    :bind
    ("C-=" . text-scale-increase)
    ("C--" . text-scale-decrease)
    ("<C-wheel-up>" . text-scale-increase)
    ("<C-wheel-down>" . text-scale-decrease))

;; window

(use-package window
    :ensure nil
    :custom
    (display-buffer-alist
        '(
             ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|[Hh]elp\\|Messages\\|Bookmark List\\|Ibuffer\\|Occur\\|eldoc.*\\)\\*"
                 (display-buffer-in-side-window)
                 (window-height . 0.3)
                 (side . bottom)
                 (slot . 0))
             ("\\*\\(Flymake diagnostics\\|xref\\|Completions\\)"
                 (display-buffer-in-side-window)
                 (window-height . 0.3)
                 (side . bottom)
                 (slot . 1)))))

;; dired

(use-package dired
    :ensure nil
    :custom
    (dired-kill-when-opening-new-dired-buffer t)
    (dired-listing-switches "-lah --group-directories-first")
    (dired-dwim-target t)
    :config
    (when (eq system-type 'darwin)
        (let ((gls (executable-find "gls")))
            (when gls
                (setq insert-directory-program gls)))))

;; eldoc

(use-package eldoc
    :ensure nil
    :custom
    (eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
    (eldoc-idle-delay 0.1)
    :config
    (global-eldoc-mode))

;; org

(use-package org :ensure nil)

;; undo-fu

(use-package undo-fu :ensure t)
(use-package undo-fu-session :ensure t :config (undo-fu-session-global-mode))

;; evil-mode

(setq evil-want-empty-ex-last-command nil)
(setq evil-want-keybinding nil)
(setq evil-want-C-u-scroll t)
(setq evil-want-C-d-scroll t)
(setq evil-want-integration t)
(setq evil-want-fine-undo t)
(setq evil-vsplit-window-right t)
(setq evil-auto-balance-windows nil)
(use-package
    evil
    :ensure t
    :init (evil-mode)
    :custom
    (evil-leader/in-all-states t)
    (evil-undo-system 'undo-fu)
    :config
    (evil-set-leader 'normal (kbd "SPC"))
    (evil-set-leader 'normal "\\" t)

    (evil-define-key 'insert 'global (kbd "\C-y") nil)
    (evil-define-key 'insert 'global (kbd "TAB") 'tab-to-tab-stop)

    (evil-define-key 'normal 'global (kbd "M-h") 'evil-window-left)
    (evil-define-key 'normal 'global (kbd "M-l") 'evil-window-right)
    (evil-define-key 'normal 'global (kbd "M-k") 'evil-window-up)
    (evil-define-key 'normal 'global (kbd "M-j") 'evil-window-down)

    (evil-define-key '(normal motion visual) 'global (kbd "H") 'evil-first-non-blank)
    (evil-define-key '(normal motion visual) 'global (kbd "L") 'evil-end-of-line)

    (evil-define-key 'normal 'global (kbd "<leader>tn") 'global-display-line-numbers-mode)

    (evil-define-key 'normal 'global (kbd "<leader>w") 'evil-write)
    (evil-define-key 'normal 'global (kbd "<leader>a") 'evil-write-all)
    (evil-define-key 'normal 'global (kbd "<leader>d") 'kill-current-buffer)
    (evil-define-key 'normal 'global (kbd "<leader>q") 'evil-window-delete)

    (defmacro evil-map (state key seq)
        "Map for a given STATE a KEY to a sequence SEQ of keys.

        Can handle recursive definition only if KEY is the first key of
        SEQ, and if KEY's binding in STATE is defined as a symbol in
        `evil-normal-state-map'.
        Example: (evil-map visual \"<\" \"<gv\")"
        (let ((map (intern (format "evil-%S-state-map" state)))
                 (key-cmd (lookup-key evil-normal-state-map key)))
            `(define-key ,map ,key
                 (lambda ()
                     (interactive)
                     ,(if (string-equal key (substring seq 0 1))
                          `(let ((orig-this-command this-command))
                               (setq this-command ',key-cmd)
                               (call-interactively ',key-cmd)
                               (run-hooks 'post-command-hook)
                               (setq this-command orig-this-command)
                               (execute-kbd-macro ,(substring seq 1)))
                          (execute-kbd-macro ,seq))))))

    (evil-map visual "<" "<gv")
    (evil-map visual ">" ">gv")

    (defvar my-intercept-mode-map (make-sparse-keymap)
        "High precedence keymap.")
    (define-minor-mode my-intercept-mode
        "Global minor mode for higher precedence evil keybindings."
        :global t)
    (my-intercept-mode)
    (dolist (state '(normal))
        (evil-make-intercept-map
            (evil-get-auxiliary-keymap
                my-intercept-mode-map state t t)
            state))

    (defun eldoc-and-switch ()
        "Show hover documentation and jump to *eldoc* buffer."
        (interactive)
        (eldoc t)
        (let ((help-buffer "*eldoc*"))
            (when (get-buffer help-buffer)
                (switch-to-buffer-other-window help-buffer))))
    (evil-define-key
        'normal my-intercept-mode-map (kbd "K") 'eldoc-and-switch)

    (evil-define-command +evil:cd (&optional path)
        "Change `default-directory' with `cd'."
        (interactive "<f>")
        (let ((path (or path "~")))
            (evil-ex-define-cmd "cd" #'+evil:cd)
            (cd path)
            (message "Changed directory to '%s'" (abbreviate-file-name (expand-file-name path)))))
    (evil-ex-define-cmd "cd" #'+evil:cd)

    (defun my-evil-yank-advice (orig-fn beg end &rest args)
        (pulse-momentary-highlight-region beg end)
        (apply orig-fn beg end args))
    (advice-add 'evil-yank :around 'my-evil-yank-advice))
(use-package
    evil-collection
    :ensure t
    :after evil
    :custom
    (evil-collection-want-find-usages-bindings t)
    :config
    ;; (setq evil-collection-mode-list
    ;;     '(dired ibuffer imenu magit vertico consult eglot eldoc corfu help elpaca compile eshell help))
    (evil-collection-init))
(use-package
    evil-commentary
    :ensure t
    :config (evil-commentary-mode))

;; drag-stuff

(use-package drag-stuff
    :ensure t
    :config
    (define-key evil-visual-state-map (kbd "J") 'drag-stuff-down)
    (define-key evil-visual-state-map (kbd "K") 'drag-stuff-up))

;; magit

(elpaca transient)
(elpaca (magit :wait t))

;; EditorConfig

(use-package editorconfig :ensure t :config (editorconfig-mode 1))

;; flymake

(use-package
    flymake
    :ensure nil
    :config
    (setq flymake-indicator-type 'fringes)
    (evil-define-key
        'normal my-intercept-mode-map (kbd "[d") 'flymake-goto-prev-error)
    (evil-define-key
        'normal my-intercept-mode-map (kbd "]d") 'flymake-goto-next-error))

;; Markdown

(use-package
    markdown-mode
    :ensure t
    :mode ("README\\.md\\'" . gfm-mode)
    :init (setq markdown-command "multimarkdown"))

;; GLSL

(use-package glsl-mode
    :ensure t
    :mode ("\\.shader\\'" "\\.glsl\\'"))

;; Godot

(use-package gdscript-mode
    :ensure (gdscript-mode
                :host github
                :repo "godotengine/emacs-gdscript-mode"
                :inherit nil
                :after treesit)
    :config
    (setq gdscript-eglot-version "4.4")
    (setq gdscript-indent-offset 4)
    (setq gdscript-use-tab-indents nil))
(use-package gdshader-mode
    :ensure (gdshader-mode
                :host github
                :repo "bbbscarter/gdshader-mode"
                :inherit nil
                :after glsl-mode)
    :after cape
    :init
    (defvar my-gdshader-keywords (regexp-opt '("instance" "varying") 'symbols))
    (defun gdshader-config()
        (interactive)
        (font-lock-add-keywords nil `((,my-gdshader-keywords . glsl-keyword-face)))
        (setq-local completion-at-point-functions (list (cape-capf-super #'cape-dabbrev #'cape-keyword))))
    :hook (gdshader-mode . gdshader-config)
    :config
    (with-eval-after-load 'cape-keyword (add-to-list 'cape-keyword-list (append '(gdshader-mode) gdshader-all-keywords))))

;; Treesitter

(defface treesit-custom-boolean-face
    '((t :inherit font-lock-constant-face :weight bold))
    "Custom face for tree-sitter. Sometimes used with 'true', 'false' keywords."
    :group 'font-lock-faces)

(defface treesit-custom-null-face
    '((t :inherit font-lock-builtin-face))
    "Custom face for tree-sitter. Sometimes used with 'NULL' or 'nullptr'."
    :group 'font-lock-faces)

(defface treesit-custom-special-keyword-1-face
    '((t :inherit font-lock-keyword-face))
    "Custom face for tree-sitter. Sometimes used with 'this' keywords."
    :group 'font-lock-faces)

(defface treesit-custom-special-keyword-2-face
    '((t :inherit font-lock-keyword-face))
    "Custom face for tree-sitter. Sometimes used with 'return' keywords."
    :group 'font-lock-faces)

(defface treesit-custom-enumerator-face
    '((t :inherit font-lock-constant-face))
    "Custom enumerator face for tree-sitter"
    :group 'font-lock-faces)

(defface treesit-custom-field-face
    '((t :inherit font-lock-property-name-face))
    "Custom field face for tree-sitter."
    :group 'font-lock-faces)

(defface treesit-custom-parameter-face
    '((t :inherit font-lock-variable-name-face))
    "Custom parameter face for tree-sitter."
    :group 'font-lock-faces)

(defface treesit-custom-named-operator-face
    '((t :inherit font-lock-operator-face :weight bold))
    "Custom 'new' face for C/C++ tree-sitter."
    :group 'font-lock-faces)

(defface treesit-custom-label-face
    '((t :inherit font-lock-keyword-face :weight semibold :slant normal))
    "Custom label face for C/C++ tree-sitter."
    :group 'font-lock-faces)

(defvar treesit-custom-constant-regex "\\`[A-Z_][A-Z0-9_]+\\'" "")

(use-package
    treesit
    :ensure nil
    :config
    (setq treesit-font-lock-level 4))
(use-package
    treesit-auto
    :ensure t
    :custom (treesit-auto-install 't)
    :config
    (setq treesit-auto-gdscript-config
        (make-treesit-auto-recipe
            :lang 'gdscript
            :ts-mode 'gdscript-ts-mode
            :remap 'gdscript-mode
            :url "https://github.com/PrestonKnopp/tree-sitter-gdscript"
            :ext "\\.gd\\'"))
    (add-to-list 'treesit-auto-recipe-list treesit-auto-gdscript-config)
    (setq treesit-auto-langs
        '(c cpp cmake toml yaml gdscript lua))
    (treesit-auto-install-all)
    (global-treesit-auto-mode))
(use-package
    cmake-ts-mode
    :ensure nil
    :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))
(use-package c-ts-mode
    :ensure nil
    :preface
    (defun my-c-ts-indent-style()
        `(((node-is "preproc") column-0 0)
             ((n-p-gp nil "declaration_list" "namespace_definition") parent-bol 0)
             ,@(alist-get 'bsd (c-ts-mode--indent-styles 'cpp))))
    (defun my-c-ts-keywords (orig-fun &rest args)
        `("#if" "#ifdef" "#ifndef" "#elif" "#else" "#endif" "#define",@(apply orig-fun args)))
    :config
    (setq c-ts-mode-indent-offset 4)
    (setq c-ts-mode-indent-style #'my-c-ts-indent-style)
    (setq c-ts-mode-enable-doxygen t)
    (setq c-ts-mode--preproc-keywords '("#include"))
    (advice-add 'c-ts-mode--keywords :around #'my-c-ts-keywords)
    (defvar my-c-ts-mode-constants
        `(
             ((field_identifier) @font-lock-constant-face (:match ,treesit-custom-constant-regex @font-lock-constant-face))
             ((identifier) @font-lock-constant-face (:match ,treesit-custom-constant-regex @font-lock-constant-face))))
    (defvar my-c-ts-mode-common
        `(
             (parameter_declaration declarator: (identifier) @treesit-custom-parameter-face)
             (parameter_declaration declarator: (pointer_declarator declarator: (identifier) @treesit-custom-parameter-face))
             (parameter_declaration declarator: (pointer_declarator declarator: (pointer_declarator declarator: (identifier) @treesit-custom-parameter-face)))
             (parameter_declaration declarator: (pointer_declarator declarator: (pointer_declarator declarator: (pointer_declarator declarator: (identifier) @treesit-custom-parameter-face))))
             (parameter_declaration declarator: (function_declarator declarator: (parenthesized_declarator (_ declarator: (identifier) @treesit-custom-parameter-face))))
             (field_identifier) @treesit-custom-field-face
             (conditional_expression (["?" ":"]) @font-lock-operator-face)
             [(true) (false)] @treesit-custom-boolean-face
             (null) @treesit-custom-null-face
             (char_literal "'" @font-lock-string-face)
             (escape_sequence) @treesit-custom-named-operator-face
             (case_statement value: (identifier) @font-lock-constant-face)
             (sizeof_expression "sizeof" @treesit-custom-named-operator-face)
             (labeled_statement label: (_) @treesit-custom-label-face)
             (goto_statement label: (_) @treesit-custom-label-face)
             ("return" @treesit-custom-special-keyword-2-face)
             (sized_type_specifier) @font-lock-builtin-face
             (primitive_type) @font-lock-builtin-face
             (enumerator
                 name: (identifier) @treesit-custom-enumerator-face)))
    (defvar my-c-ts-mode-preprocessor
        `(
             (call_expression function: (identifier) @font-lock-function-call-face)
             (call_expression function: (field_expression field: (field_identifier) @font-lock-function-call-face))
             (preproc_call directive: (_) @font-lock-keyword-face)
             (preproc_defined
                 "defined" @font-lock-function-call-face
                 "(" @font-lock-punctuation-face
                 (identifier) @font-lock-constant-face
                 ")" @font-lock-punctuation-face)
             (preproc_def name: (_) @font-lock-constant-face)
             (preproc_function_def name: (_) @font-lock-function-name-face)
             (preproc_ifdef name: (_) @font-lock-constant-face)
             (preproc_params
                 "(" @font-lock-punctuation-face
                 (identifier) @treesit-custom-parameter-face
                 ")" @font-lock-punctuation-face)))
    (add-hook 'c-ts-mode-hook
        (lambda()
            (add-to-list 'treesit-font-lock-settings
                (car (treesit-font-lock-rules
                         :language 'c
                         :override t
                         :feature 'constants
                         my-c-ts-mode-constants)) t)
            (add-to-list 'treesit-font-lock-settings
                (car (treesit-font-lock-rules
                         :language 'c
                         :override t
                         :feature 'common
                         my-c-ts-mode-common)) t)
            (add-to-list 'treesit-font-lock-settings
                (car (treesit-font-lock-rules
                         :language 'c
                         :override t
                         :feature 'preprocessor
                         my-c-ts-mode-preprocessor)) t)
            (add-to-list 'treesit-font-lock-settings
                (car (treesit-font-lock-rules
                         :language 'c
                         :override t
                         :feature 'c-only-overrides
                         `((macro_type_specifier name: (identifier) @font-lock-function-call-face)))) t)))
    (add-hook 'c++-ts-mode-hook
        (lambda()
            (add-to-list 'treesit-font-lock-settings
                (car (treesit-font-lock-rules
                         :language 'cpp
                         :override t
                         :feature 'common
                         my-c-ts-mode-common)) t)
            (add-to-list 'treesit-font-lock-settings
                (car (treesit-font-lock-rules
                         :language 'cpp
                         :override t
                         :feature 'fields
                         `(
                              (function_declarator declarator: ([(field_identifier) (identifier)]) @font-lock-function-name-face)
                              (field_declaration type: (placeholder_type_specifier (auto)) declarator: (field_identifier) @font-lock-function-name-face)))) t)
            (add-to-list 'treesit-font-lock-settings
                (car (treesit-font-lock-rules
                         :language 'cpp
                         :override t
                         :feature 'constants
                         my-c-ts-mode-constants)) t)
            (add-to-list 'treesit-font-lock-settings
                (car (treesit-font-lock-rules
                         :language 'cpp
                         :override t
                         :feature 'preprocessor
                         my-c-ts-mode-preprocessor)) t)
            (add-to-list 'treesit-font-lock-settings
                (car (treesit-font-lock-rules
                         :language 'cpp
                         :override t
                         :feature 'namespace-types
                         `(
                              (using_declaration (identifier) @font-lock-type-face)
                              (using_declaration (qualified_identifier scope: (namespace_identifier) name: (identifier) @font-lock-type-face))
                              (namespace_identifier) @font-lock-type-face
                              (qualified_identifier scope: (namespace_identifier) name: (qualified_identifier scope: (namespace_identifier) name: (identifier)  @font-lock-type-face))))) t)
            (add-to-list 'treesit-font-lock-settings
                (car (treesit-font-lock-rules
                         :language 'cpp
                         :override t
                         :feature 'namespace-functions
                         `(
                              (call_expression function: (qualified_identifier scope: (namespace_identifier) name: (identifier) @font-lock-function-call-face))
                              (function_declarator declarator: (qualified_identifier scope: (namespace_identifier) name: (identifier) @font-lock-function-name-face))))) t)
            (add-to-list 'treesit-font-lock-settings
                (car (treesit-font-lock-rules
                         :language 'cpp
                         :override t
                         :feature 'cpp-overrides
                         '(
                              (declaration declarator: (function_declarator declarator: (identifier) @font-lock-function-name-face))
                              (parameter_declaration declarator: (_ (identifier) @treesit-custom-parameter-face))
                              ((this) @treesit-custom-special-keyword-1-face)
                              (concept_definition name: (_) @font-lock-type-face)
                              (template_function name: (identifier) @font-lock-function-name-face)
                              (new_expression "new" @treesit-custom-named-operator-face)
                              (delete_expression "delete" @treesit-custom-named-operator-face)
                              (function_definition type: (type_identifier) @font-lock-keyword-face (:match "\\`compl\\'" @font-lock-keyword-face))
                              (template_parameter_list (["<" ">"]) @font-lock-punctuation-face)
                              (template_parameter_list (parameter_declaration declarator: (_) @treesit-custom-parameter-face))
                              (template_argument_list (["<" ">"]) @font-lock-punctuation-face)
                              (template_argument_list (type_descriptor type: (type_identifier) @font-lock-type-face))
                              (template_type name: (type_identifier) @font-lock-type-face)
                              ("::" @font-lock-punctuation-face)
                              (call_expression
                                  function:
                                  (qualified_identifier scope: (namespace_identifier)
                                      name: (identifier) @font-lock-function-call-face))
                              (call_expression
                                  function:
                                  (template_function name: (identifier) @font-lock-function-call-face))
                              )))) t)))
(use-package gdscript-ts-mode
    :ensure nil
    :after gdscript-mode
    :config
    (defvar my-gdscript-ts-mode-punctuation '("[" "]" "(" ")" "{" "}" "," ":"))
    (defvar my-gdscript-ts-mode-operators
        '("%" "%=" "->" "." "!=" "+=" "-="
             "/=" "*=" "==" ">>" "<<" "~"
             "&" "|" "&=" "|=" "-" ">="
             "<=" "||" "&&" ">>=" "<<="
             "^="))
    (defvar my-gdscript-ts-mode-named-operators
        '("not" "in" "and" "is"))
    (defvar my-gdscript-ts-mode-types
        `((identifier) @font-lock-type-face (:match "\\`[A-Z][a-zA-Z0-9_]*[a-z][a-zA-Z0-9_]*\\'" @font-lock-type-face)))
    (defvar my-gdscript-ts-mode-builtin-classes-regex
        (rx (| "Camera2D" "Camera3D" "Control" "Node2D" "Node3D" "Vector2" "Vector2i" "Vector3" "Vector3i" "Vector4" "Vector4i" "Color" "Rect2" "Rect2i" "Array" "Basis" "Dictionary" "Plane" "Quat" "RID" "Rect3" "Transform" "Transform2D" "Transform3D" "AABB" "String" "NodePath" "PoolByteArray" "PoolIntArray" "PoolRealArray" "PoolStringArray" "PoolVector2Array" "PoolVector3Array" "PoolColorArray" "Signal" "Callable" "StringName" "Quaternion" "Projection" "PackedByteArray" "PackedInt32Array" "PackedInt64Array" "PackedFloat32Array" "PackedFloat64Array" "PackedStringArray" "PackedVector2Array" "PackedVector2iArray" "PackedVector3Array" "PackedVector3iArray" "PackedVector4Array" "PackedColorArray" "JSON" "UPNP" "OS" "IP" "JSONRPC" "XRVRS")))
    (defvar my-gdscript-ts-mode-constants
        `(
             (const_statement name: (name) @font-lock-constant-face)
             ((identifier) @font-lock-constant-face (:match ,treesit-custom-constant-regex @font-lock-constant-face))
             (variable_statement name: (name) @font-lock-constant-face (:match ,treesit-custom-constant-regex @font-lock-constant-face))))
    (defvar my-gdscript-ts-mode-overrides
        `(
             ;; ((identifier) @font-lock-builtin-face (:match ,my-gdscript-ts-mode-builtin-classes-regex @font-lock-builtin-face))
             (escape_sequence) @treesit-custom-named-operator-face
             ((identifier) @font-lock-type-face (:match ,my-gdscript-ts-mode-builtin-classes-regex @font-lock-type-face))
             (signal_statement (name) @font-lock-function-call-face)
             [(true) (false)] @treesit-custom-boolean-face
             (null) @treesit-custom-null-face
             (attribute (identifier) @treesit-custom-special-keyword-1-face (:match ,(rx (| "self")) @treesit-custom-special-keyword-1-face))
             (return_statement "return" @treesit-custom-special-keyword-2-face)
             ([,@my-gdscript-ts-mode-punctuation] @font-lock-punctuation-face)
             ([,@my-gdscript-ts-mode-operators] @font-lock-operator-face)
             ([,@my-gdscript-ts-mode-named-operators] @treesit-custom-named-operator-face)
             (enum_definition name: (_) @font-lock-type-face)
             (enumerator left: (identifier) @treesit-custom-enumerator-face)
             (annotation "@" @font-lock-preprocessor-face (identifier) @font-lock-preprocessor-face)))
    (add-hook 'gdscript-ts-mode-hook
        (lambda()
            (add-to-list 'treesit-font-lock-settings
                (car (treesit-font-lock-rules
                         :language 'gdscript
                         :override t
                         :feature 'better-types
                         `(,my-gdscript-ts-mode-types))) t)
            (add-to-list 'treesit-font-lock-settings
                (car (treesit-font-lock-rules
                         :language 'gdscript
                         :override t
                         :feature 'better-constants
                         my-gdscript-ts-mode-constants)) t)
            (add-to-list 'treesit-font-lock-settings
                (car (treesit-font-lock-rules
                         :language 'gdscript
                         :override t
                         :feature 'overrides
                         my-gdscript-ts-mode-overrides)) t))))

;; LSP

(use-package
    eglot
    :ensure t
    :hook
    ((c-ts-mode c++-ts-mode rust-ts-mode gdscript-ts-mode) . eglot-ensure)
    :custom
    (eglot-mode-line-format '(eglot-mode-line-menu eglot-mode-line-session eglot-mode-line-action-suggestion))
    (eglot-ignored-server-capabilities '(:inlayHintProvider :documentHighlightProvider))
    (eglot-events-buffer-size 0)
    (eglot-autoshutdown t)
    (eglot-report-progress nil)
    ;; (eglot-stay-out-of '(flymake eldoc))
    :config
    (setq jsonrpc-event-hook nil)
    (setq eglot-events-buffer-config '(:size 0 :format full))
    ;; (setq eldoc-idle-delay 0.1)
    ;; (add-to-list 'eglot-server-programs
    ;;              `(cmake-ts-mode . ("~/.local/bin/cmake-language-server")))
    ;; (add-to-list 'eglot-server-programs
    ;;              `(glsl-mode . ("~/.config/emacs/lsp-servers/glsl_analyzer/glsl_analyzer"))))
    (set-face-attribute 'eglot-mode-line nil :inherit 'mode-line-buffer-id :weight 'normal)

    (evil-define-key
        'normal my-intercept-mode-map (kbd "grn") 'eglot-rename)
    (evil-define-key
        'normal my-intercept-mode-map (kbd "gra") 'eglot-code-actions)
    (evil-define-key
        'normal my-intercept-mode-map (kbd "<leader>cf") 'eglot-format))
(use-package eglot-inactive-regions
    :ensure t
    :custom
    (eglot-inactive-regions-style 'shadow-face)
    :config
    (eglot-inactive-regions-mode 1))

;; Completion

(use-package
    consult
    :ensure t
    :custom
    (consult-line-start-from-top t)
    :config
    (evil-define-key 'normal 'global (kbd "gO") 'consult-imenu)
    (evil-define-key 'normal 'global (kbd "<leader>/") 'consult-line)
    (evil-define-key 'normal 'global (kbd "<leader>sg") 'consult-ripgrep)
    (evil-define-key 'normal 'global (kbd "<leader>sf") 'project-find-file)
    (evil-define-key 'normal 'global (kbd "<leader>SPC") 'consult-buffer))
(use-package
    consult-eglot
    :ensure t
    :config
    (evil-define-key 'normal 'global (kbd "gW") 'consult-eglot-symbols))
(use-package
    vertico
    :ensure t
    :config
    (setq vertico-cycle t)
    (setq vertico-resize nil)
    (vertico-mode 1))
(use-package hotfuzz
    :ensure (hotfuzz :host github :url "axelf4/hotfuzz"))
(use-package
    orderless
    :after hotfuzz
    :ensure t
    :custom
    ;; (orderless-style-dispatchers '(orderless-affix-dispatch))
    ;; (orderless-component-separator #'orderless-escapable-split-on-space)
    (completion-styles '(orderless hotfuzz basic))
    (completion-category-defaults nil)
    (completion-category-overrides
        '(
             (eglot (styles orderless))
             (eglot-capf (styles orderless))
             (file (styles basic partial-completion))
             (buffer (styles orderless))
             (project-file (styles hotfuzz))
             (command (styles orderless)))))
(use-package marginalia :ensure t :init (marginalia-mode))
(use-package corfu
    :ensure t
    :custom
    (corfu-left-margin-width 0.0)
    (corfu-right-margin-width 0.0)
    (corfu-bar-width 0.0)
    (corfu-cycle t)
    (corfu-auto t)
    (corfu-auto-delay 0.2)
    (corfu-auto-prefix 2)
    (corfu-popupinfo-mode t)
    (corfu-popupinfo-delay 0.25)
    :config
    (add-hook 'eshell-mode-hook
        (lambda ()
            (setq-local corfu-auto nil)
            (corfu-mode)))
    (global-corfu-mode)
    :bind (:map corfu-map ("C-y" . corfu-complete)))
(use-package cape
    :ensure t
    :custom
    (cape-dabbrev-check-other-buffers nil)
    :config
    (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
    (add-to-list 'completion-at-point-functions #'cape-file)
    (add-to-list 'completion-at-point-functions #'cape-elisp-block))
