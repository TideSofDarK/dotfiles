;; Init GC

(setq gc-cons-threshold (* 50 1000 1000))

;; Maximize on launch

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Add Lisp Path

(add-to-list 'load-path (concat user-emacs-directory "lisp"))

;; Elpaca Package Manager

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
(elpaca elpaca-use-package (elpaca-use-package-mode))

;; Set Font

(let ((mono-spaced-font "Sarasa Term Slab CL Nerd Font")
         (proportionately-spaced-font "Sarasa UI Nerd Font"))
    (set-face-attribute 'default nil
        :family mono-spaced-font
        :height 120)
    (set-face-attribute 'fixed-pitch nil
        :family mono-spaced-font
        :height 1.0)
    (set-face-attribute 'variable-pitch nil
        :family proportionately-spaced-font
        :height 1.0))

;; Themes

(setq kanagawa-themes-keyword-italic t)
(setq kanagawa-themes-comment-italic nil)
(require 'kanagawa-themes)
(load-theme 'kanagawa-wave :no-confirm-loading)

;; Cleaner mode-line

(use-package minions
    :ensure t
    :config (minions-mode 1))

;; Basic Configuration

(use-package
    emacs
    :ensure nil
    :custom
    (menu-bar-mode nil)
    (scroll-bar-mode nil)
    (tool-bar-mode nil)
    (inhibit-startup-screen t)
    (inhibit-splash-screen t)
    (initial-scratch-message nil)

    (delete-selection-mode t)
    (electric-indent-mode nil)
    (electric-pair-mode t)

    (blink-cursor-mode nil)
    (global-auto-revert-mode t)

    (dired-kill-when-opening-new-dired-buffer t)
    (recentf-mode t)

    (truncate-lines t)

    (text-mode-ispell-word-completion nil)
    (read-extended-command-predicate
        #'command-completion-default-include-p)

    (display-line-numbers-grow-only t)
    (display-line-numbers-type 'relative)
    (display-line-numbers-width-start t)
    (global-display-line-numbers-mode t)

    (mouse-wheel-progressive-speed nil)
    (scroll-conservatively 10)
    (scroll-margin 8)

    (indent-tabs-mode nil)
    (tab-width 4)
    (sgml-basic-offset 4)

    (use-dialog-box nil)
    (confirm-kill-processes nil)
    (set-buffer-modified-p nil)

    (make-backup-files nil)
    (auto-save-default nil)

    (enable-recursive-minibuffers t)
    (minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))

    :hook (prog-mode . (lambda () (hs-minor-mode t)))
    :config
    (set-display-table-slot standard-display-table 0 ?\ )
    (setq eldoc-echo-area-use-multiline-p nil)
    (fset 'display-startup-echo-area-message 'ignore)
    (set-fringe-mode 0)
    (savehist-mode 1)
    (fset 'yes-or-no-p 'y-or-n-p)
    (setq custom-file (locate-user-emacs-file "custom-vars.el"))
    (load custom-file 'noerror 'nomessage)
    :bind
    ("C-=" . text-scale-increase)
    ("C--" . text-scale-decrease)
    ("<C-wheel-up>" . text-scale-increase)
    ("<C-wheel-down>" . text-scale-decrease))
;; (([escape] . keyboard-escape-quit)))
;;  ("C-g" . evil-normal-state)))

;; undo-tree

(use-package
    undo-tree
    :ensure t
    :demand t
    :custom (undo-tree-mode-lighter "")
    :custom
    (undo-tree-history-directory-alist
        `((".*" . ,(expand-file-name "undo-tree/" user-emacs-directory))))
    :config (global-undo-tree-mode))

;; evil-mode

(use-package
    evil
    :ensure t
    :init (evil-mode)
    :custom
    (evil-undo-system 'undo-tree)
    (evil-ex-previous-command nil)
    (evil-want-empty-ex-last-command t)
    (evil-want-C-u-scroll t)
    (evil-want-C-d-scroll t)
    (evil-want-integration t)
    (evil-want-keybinding nil)
    :config
    (evil-set-leader 'normal (kbd "SPC"))
    (evil-set-leader 'normal "\\" t)

    (evil-define-key 'normal 'global (kbd "K") 'eldoc)

    (evil-define-key 'insert 'global (kbd "\C-y") nil)

    (define-key evil-normal-state-map (kbd "M-h") 'evil-window-left)
    (define-key evil-normal-state-map (kbd "M-l") 'evil-window-right)
    (define-key evil-normal-state-map (kbd "M-k") 'evil-window-up)
    (define-key evil-normal-state-map (kbd "M-j") 'evil-window-down)

    (evil-define-key
        '(normal motion visual)
        'global
        (kbd "H")
        'evil-beginning-of-line
        (kbd "L")
        'evil-end-of-line)

    (define-key
        evil-normal-state-map
        (kbd "<leader>tn")
        'global-display-line-numbers-mode)

    (define-key evil-normal-state-map (kbd "<leader>w") 'evil-write)
    (define-key evil-normal-state-map (kbd "<leader>a") 'evil-write-all)
    (define-key
        evil-normal-state-map (kbd "<leader>d") 'evil-delete-buffer)
    (define-key
        evil-normal-state-map (kbd "<leader>q") 'evil-window-delete)
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
            state)))
(use-package
    evil-collection
    :ensure t
    :after evil
    :config
    (evil-define-command +evil:cd (&optional path)
        "Change `default-directory' with `cd'."
        (interactive "<f>")
        (let ((path (or path "~")))
            (evil-ex-define-cmd "cd" #'+evil:cd)
            (cd path)
            (message "Changed directory to '%s'" (abbreviate-file-name (expand-file-name path)))))
    (evil-ex-define-cmd "cd" #'+evil:cd)

    ;; (setq evil-collection-mode-list '(dired ibuffer magit corfu vertico consult vterm))
    (setq evil-collection-mode-list
        '(dired ibuffer magit vertico consult eldoc company help))
    (evil-collection-init))
(use-package
    evil-commentary
    :ensure t
    :after evil
    :config (evil-commentary-mode))

(defun my/evil-yank-advice (orig-fn beg end &rest args)
    (pulse-momentary-highlight-region beg end)
    (apply orig-fn beg end args))
(advice-add 'evil-yank :around 'my/evil-yank-advice)

;; drag-stuff

(use-package drag-stuff
    :ensure t
    :after evil
    :config
    (define-key evil-visual-state-map (kbd "J") 'drag-stuff-down)
    (define-key evil-visual-state-map (kbd "K") 'drag-stuff-up))

;; magit

(elpaca transient)
(elpaca (magit :wait t))

;; EditorConfig

(use-package editorconfig :ensure t :config (editorconfig-mode 1))

;; LSP

(setenv "LSP_USE_PLISTS" "true")
(use-package lsp-mode
    :ensure t
    :custom
    ;; (lsp-completion-enable-additional-text-edit nil)
    ;; (lsp-enable-xref nil)
    (lsp-auto-guess-root t)
    (lsp-keep-workspace-alive nil)
    (lsp-inlay-hint-enable nil)
    (lsp-update-inlay-hints-on-scroll nil)
    (lsp-enable-folding nil)
    (lsp-signature-auto-activate t)
    (lsp-signature-doc-lines 1)
    (lsp-enable-links nil)
    (lsp-eldoc-enable-hover t)
    (lsp-eldoc-prefer-signature-help t)
    (lsp-eldoc-render-all t)
    (lsp-enable-folding nil)
    (lsp-enable-dap-auto-configure nil)
    (lsp-display-inline-image nil)
    (lsp-auto-execute-action nil)
    (lsp-headerline-breadcrumb-enable nil)
    (lsp-lens-enable nil)
    (lsp-semantic-tokens-enable t)
    (lsp-semantic-tokens-apply-modifiers t)
    (lsp-enable-symbol-highlighting nil)
    (lsp-enable-text-document-color nil)
    (lsp-enable-on-type-formatting nil)
    (lsp-modeline-code-actions-enable nil)
    (lsp-modeline-diagnostics-enable nil)
    (lsp-keymap-prefix nil)
    :hook (
              (c-ts-mode . lsp)
              (c++-ts-mode . lsp)
              (rust-ts-mode . lsp)
              (gdscript-ts-mode . lsp))
    :commands lsp
    :config
    (setq lsp-semantic-token-faces
        '(
             ("bracket" . font-lock-punctuation-face)
             ("comment" . lsp-face-semhl-comment)
             ;; ("keyword" . lsp-face-semhl-keyword)
             ("string" . lsp-face-semhl-string)
             ("number" . lsp-face-semhl-number)
             ("regexp" . lsp-face-semhl-regexp)
             ;; ("operator" . lsp-face-semhl-operator)
             ("namespace" . lsp-face-semhl-namespace)
             ;; ("type" . lsp-face-semhl-type)
             ("struct" . lsp-face-semhl-struct)
             ("class" . lsp-face-semhl-class)
             ("interface" . lsp-face-semhl-interface)
             ("enum" . lsp-face-semhl-enum)
             ("typeParameter" . lsp-face-semhl-type-parameter)
             ("function" . lsp-face-semhl-function)
             ("method" . lsp-face-semhl-method)
             ("member" . lsp-face-semhl-member)
             ("property" . lsp-face-semhl-property)
             ("event" . lsp-face-semhl-event)
             ("macro" . lsp-face-semhl-macro)
             ;; ("variable" . lsp-face-semhl-variable)
             ("parameter" . lsp-face-semhl-parameter)
             ("label" . lsp-face-semhl-label)
             ("enumConstant" . lsp-face-semhl-constant)
             ("enumMember" . lsp-face-semhl-constant)
             ("dependent" . lsp-face-semhl-type)
             ("concept" . lsp-face-semhl-interface)))
    (setq lsp-semantic-token-modifier-faces
        '(
             ;; ("readonly" . lsp-face-semhl-constant)
             ("method.defaultLibrary" . lsp-face-semhl-default-library)
             ;; ("defaultLibrary" . lsp-face-semhl-default-library)
             ;; ("declaration" . lsp-face-semhl-constant)
             ;; ("definition" . lsp-face-semhl-constant)
             ;; ("implementation" . lsp-face-semhl-constant)
             ;; ("static" . lsp-face-semhl-static)
             ;; ("deprecated" . lsp-face-semhl-deprecated)
             ;; ("abstract" . lsp-face-semhl-keyword)
             ;; ("async" . lsp-face-semhl-macro)
             ("documentation" . lsp-face-semhl-comment)
             ;; ("classScope" . lsp-face-semhl-member)
             ;; ("namespaceScope" . lsp-face-semhl-namespace-scope)
             ("modification" . lsp-face-semhl-operator)))
    (evil-define-key
        'normal my-intercept-mode-map (kbd "grn") 'lsp-rename)
    ;; (evil-define-key
    ;;     'normal my-intercept-mode-map (kbd "gra") 'lsp-code-actions)
    (evil-define-key
        'normal my-intercept-mode-map (kbd "<leader>cf") 'lsp-format-buffer))
;; (use-package
;;     eglot
;;     :ensure nil
;;     :hook
;;     ((c-ts-mode c++-ts-mode rust-ts-mode gdscript-ts-mode) . eglot-ensure)
;;     :custom
;;     (eglot-ignored-server-capabilities
;;         '(:inlayHintProvider :documentHighlightProvider))
;;     (eglot-events-buffer-size 0)
;;     (eglot-autoshutdown t)
;;     (eglot-report-progress nil)
;;     ;; (eglot-stay-out-of '(flymake eldoc))
;;     :config
;;     ;; (setq eldoc-idle-delay 0.1)
;;     ;; (add-to-list 'eglot-server-programs
;;     ;;              `(cmake-ts-mode . ("~/.local/bin/cmake-language-server")))
;;     ;; (add-to-list 'eglot-server-programs
;;     ;;              `(glsl-mode . ("~/.config/emacs/lsp-servers/glsl_analyzer/glsl_analyzer"))))
;;     (evil-define-key
;;         'normal my-intercept-mode-map (kbd "grn") 'eglot-rename)
;;     (evil-define-key
;;         'normal my-intercept-mode-map (kbd "gra") 'eglot-code-actions)
;;     (evil-define-key
;;         'normal my-intercept-mode-map (kbd "<leader>cf") 'eglot-format))

;; flymake

(use-package
    flymake
    :ensure nil
    :config (setq flymake-indicator-type 'fringes)
    (evil-define-key
        'normal my-intercept-mode-map (kbd "[d") 'flymake-goto-prev-error)
    (evil-define-key
        'normal my-intercept-mode-map (kbd "]d") 'flymake-goto-next-error))

;; Treesitter

(use-package
    treesit
    :ensure nil
    :config (setq treesit-font-lock-level 4))
(use-package
    treesit-auto
    :ensure t
    :demand t
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
        '(c cpp cmake toml yaml commonlisp gdscript lua))
    (treesit-auto-install-all)
    (global-treesit-auto-mode))
(use-package
    cmake-ts-mode
    :ensure nil
    :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

;; Markdown

(use-package
    markdown-mode
    :ensure t
    :mode ("README\\.md\\'" . gfm-mode)
    :init (setq markdown-command "multimarkdown")
    :bind (:map markdown-mode-map ("C-c C-e" . markdown-do)))

;; GLSL

(use-package glsl-mode :ensure t :mode ("\\.shader\\'" "\\.glsl\\'"))

;; Godot

(elpaca (gdscript-mode
            :host github
            :repo "godotengine/emacs-gdscript-mode"
            :inherit nil)
    :config
    (setq gdscript-indent-offset 4)
    (setq gdscript-use-tab-indents nil))

;; Completion

(use-package
    company
    :ensure t
    :custom
    (company-icon-margin 3)
    (company-tooltip-align-annotations t)
    (company-selection-wrap-around t)
    :bind
    (:map
        company-active-map
        ("C-y" . company-complete-selection)
        ("RET" . nil)
        ("<return>" . nil))
    :config
    (setq company-frontends '(company-pseudo-tooltip-frontend))
    (global-company-mode))
(use-package
    consult
    :ensure t
    :custom
    (consult-line-start-from-top t)
    :config
    (defun consult-grep-file ()
        "Search with 'grep' in current file."
        (interactive)
        (consult-grep (list (shell-quote-argument buffer-file-name))))
    (defun consult-ripgrep-file ()
        "Search with 'rg' in current file."
        (interactive)
        (let ((consult-project-function (lambda (x) nil)))
            (consult-ripgrep (list (shell-quote-argument buffer-file-name)))))
    (evil-define-key 'normal 'global (kbd "<leader>/") 'consult-line)
    (define-key evil-normal-state-map (kbd "<leader>sg") 'consult-ripgrep)
    (define-key evil-normal-state-map (kbd "<leader>sf") 'project-find-file)
    (define-key
        evil-normal-state-map (kbd "<leader>SPC") 'consult-buffer))
(use-package
    consult-lsp
    :ensure t
    :config
    (define-key
        evil-normal-state-map (kbd "gW") 'consult-lsp-symbols)
    (define-key
        evil-normal-state-map (kbd "gO") 'consult-lsp-file-symbols))
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
             (file (styles basic partial-completion))
             (buffer (styles orderless))
             (project-file (styles hotfuzz))
             (command (styles orderless)))))
(use-package marginalia :ensure t :init (marginalia-mode))
;; (use-package
;;     affe
;;     :ensure t
;;     :config
;;     (define-key evil-normal-state-map (kbd "<leader>sg") 'affe-grep)
;;     (define-key evil-normal-state-map (kbd "<leader>sf") 'affe-find)
;;     (setq affe-find-command "fd --color=never --full-path"))
;;     ; (defun affe-orderless-regexp-compiler (input _type _ignorecase)
;;     ;     (setq input (cdr (orderless-compile input)))
;;     ;     (cons input (apply-partially #'orderless--highlight input t)))
;;     ; (setq affe-regexp-compiler #'affe-orderless-regexp-compiler)
;;     ; (consult-customize affe-grep :preview-key "M-."))
;; (use-package corfu
;;     :ensure t
;;     :custom
;;     (corfu-left-margin-width 0.0)
;;     (corfu-right-margin-width 0.0)
;;     (corfu-cycle t)
;;     (corfu-auto t)
;;     (corfu-auto-prefix 2)
;;     (corfu-popupinfo-mode t)
;;     (corfu-popupinfo-delay 0.5)
;;     (corfu-preselect 'prompt)
;;     ;; (corfu-on-exact-match nil)
;;     (corfu-scroll-margin 1)
;;     (completion-ignore-case t)
;;     ;; (tab-always-indent 'complete)
;;     :init
;;     (global-corfu-mode)
;;     :bind (:map corfu-map ("C-y" . corfu-complete)))
;; (use-package cape
;;     :ensure t
;;     :init
;;     (add-to-list 'completion-at-point-functions #'cape-dabbrev)
;;     (add-to-list 'completion-at-point-functions #'cape-dict)
;;     (add-to-list 'completion-at-point-functions #'cape-file)
;;     (add-to-list 'completion-at-point-functions #'cape-elisp-block)
;;     (add-to-list 'completion-at-point-functions #'cape-keyword))

;; Runtime GC

(setq gc-cons-threshold (* 2 1000 1000))
(setq read-process-output-max (* 1024 1024))
