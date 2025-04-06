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
       ;; (proportionately-spaced-font "Sarasa UI CL"))
       (proportionately-spaced-font "Sarasa Term Slab CL"))
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
  :config (minions-mode t))

;; emacs

(use-package
  emacs
  :ensure nil
  :custom
  (native-comp-jit-compilation-deny-list '(".*-loaddefs.el.gz"))
  (fringe-mode 0) ;; 4
  (delete-by-moving-to-trash t)
  (use-short-answers t)
  (frame-inhibit-implied-resize t)
  (frame-resize-pixelwise t)
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
  (blink-cursor-mode nil)
  (revert-without-query (list "."))
  (auto-revert-stop-on-user-input nil)
  (auto-revert-verbose t)
  (global-auto-revert-non-file-buffers t)
  (global-auto-revert-ignore-modes '(Buffer-menu-mode))
  (truncate-lines t)
  (text-mode-ispell-word-completion nil)
  (read-extended-command-predicate
    #'command-completion-default-include-p)
  (display-line-numbers-type 'relative)
  (display-line-numbers-grow-only t)
  (display-line-numbers-width 3)
  (display-line-numbers-width-start t)
  ;; (global-display-line-numbers-mode t)
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
  (compilation-scroll-output t)
  :init
  (defun suppress-messages (func &rest args)
    (cl-flet ((silence (&rest args1) (ignore)))
      (advice-add 'message :around #'silence)
      (unwind-protect
        (apply func args)
        (advice-remove 'message #'silence))))
  (advice-add 'recentf-cleanup :around #'suppress-messages)
  (advice-add 'recentf-mode :around #'suppress-messages)
  :config
  (let ((inhibit-message t))
    (recentf-mode t)
    (global-auto-revert-mode t)
    (savehist-mode t)
    (delete-selection-mode t)
    (electric-indent-mode t)
    (electric-pair-mode t))
  ;; (defun skip-these-buffers (_window buffer _bury-or-kill)
  ;;     "Function for `switch-to-prev-buffer-skip'."
  ;;     (string-match "\\*[^*]+\\*" (buffer-name buffer)))
  ;; (setq switch-to-prev-buffer-skip 'skip-these-buffers)
  (add-hook 'prog-mode-hook #'display-line-numbers-mode)
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

;; ansi-color

(use-package ansi-color
  :ensure nil
  :hook (compilation-filter . ansi-color-compilation-filter))

;; Popper

(use-package popper
  :ensure t
  :after evil
  :config
  (evil-define-key 'normal 'global (kbd "<localleader>x") 'popper-toggle)
  (evil-define-key 'normal 'global (kbd "<leader>x") 'popper-toggle)
  (setq popper-mode-line "")
  (setq popper-display-control t)
  ;; (setq popper-display-function #'display-buffer-pop-up-window)
  (setq popper-reference-buffers
    '(
       ;; "^\\*.*\\*$"
       "\\*eldoc\\*"
       "^\\*godot"
       "\\*Messages\\*"
       "\\*Warnings\\*"
       "Output\\*$"
       "\\*Async Shell Command\\*"
       help-mode
       compilation-mode))
  (setq popper-window-height (lambda (win)
                               (fit-window-to-buffer
                                 win
                                 (* 2 (floor (frame-height) 5))
                                 (floor (frame-height) 3))))
  (popper-mode t))

;; dired

(use-package dired
  :ensure nil
  :custom
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-listing-switches "-lah --group-directories-first")
  (dired-dwim-target t))

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

(use-package
  evil
  :ensure t
  :custom
  (evil-leader/in-all-states t)
  (evil-undo-system 'undo-fu)
  :init
  (setq evil-want-empty-ex-last-command nil)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-d-scroll t)
  (setq evil-want-integration t)
  (setq evil-want-fine-undo t)
  (setq evil-vsplit-window-right t)
  (setq evil-auto-balance-windows nil)
  :config
  (evil-mode t)
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
  (evil-define-key 'normal 'global (kbd "<leader>tn") 'display-line-numbers-mode)
  (evil-define-key 'normal 'global (kbd "<leader>w") 'evil-write)
  (evil-define-key 'normal 'global (kbd "<leader>a") 'evil-write-all)
  (evil-define-key 'normal 'global (kbd "<leader>d") 'kill-current-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>q") 'evil-window-delete)

  (defmacro evil-map (state key seq)
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

  (defvar intercept-mode-map (make-sparse-keymap))
  (define-minor-mode intercept-mode "High precedence keymap for evil." :global t)
  (intercept-mode)
  (dolist (state '(normal))
    (evil-make-intercept-map
      (evil-get-auxiliary-keymap
        intercept-mode-map state t t)
      state))

  (defun scroll-to-center-after-goto-mark (char &optional noerror) (recenter))
  (evil--advice-add 'evil-goto-mark :after #'scroll-to-center-after-goto-mark)

  (evil-define-command +evil:cd (&optional path)
    "Change `default-directory' with `cd'."
    (interactive "<f>")
    (let ((path (or path "~")))
      (evil-ex-define-cmd "cd" #'+evil:cd)
      (cd path)
      (message "Changed directory to '%s'" (abbreviate-file-name (expand-file-name path)))))
  (evil-ex-define-cmd "cd" #'+evil:cd)

  (defun evil-yank-highlight (orig-fn beg end &rest args)
    (pulse-momentary-highlight-region beg end)
    (apply orig-fn beg end args))
  (advice-add 'evil-yank :around 'evil-yank-highlight))
(use-package
  evil-collection
  :ensure t
  :after evil
  :init
  (setq evil-collection-setup-minibuffer t)
  (setq evil-collection-want-find-usages-bindings t)
  :config
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

;; flymake

(use-package
  flymake
  :ensure nil
  :config
  (setq flymake-indicator-type 'fringes)
  (evil-define-key
    'normal intercept-mode-map (kbd "[d") 'flymake-goto-prev-error)
  (evil-define-key
    'normal intercept-mode-map (kbd "]d") 'flymake-goto-next-error))

;; EditorConfig

(use-package editorconfig :ensure t :config (editorconfig-mode 1))

;; Markdown

(use-package
  markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

;; CMake

(use-package cemako
  :ensure nil
  :after c-ts-mode
  :config
  (defun define-cemako-key(key func)
    ;; (evil-define-key 'normal c-ts-mode-map key func)
    ;; (evil-define-key 'normal c++-ts-mode-map key func)
    (evil-define-key 'normal 'global key func))
  (define-cemako-key (kbd "<leader>bt") 'cemako-select-target)
  (define-cemako-key (kbd "<leader>bc") 'cemako-run-cmake)
  (define-cemako-key (kbd "<leader>bb") 'cemako-build)
  (define-cemako-key (kbd "<leader>br") 'cemako-run))

;; Lua

(use-package lua-mode :ensure t)

;; Swift

(use-package swift-mode :ensure t)

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
  (defun gdshader-config()
    (interactive)
    (font-lock-add-keywords nil '(((regexp-opt '("instance" "varying") 'symbols) . glsl-keyword-face)))
    (setq-local completion-at-point-functions (list (cape-capf-super #'cape-dabbrev #'cape-keyword))))
  :hook (gdshader-mode . gdshader-config)
  :config
  (with-eval-after-load 'cape-keyword (add-to-list 'cape-keyword-list (append '(gdshader-mode) gdshader-all-keywords))))

;; Treesitter

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
    '(c cpp cmake toml yaml gdscript lua markdown))
  (treesit-auto-install-all)
  (global-treesit-auto-mode))
(require 'treesit-extras)
(use-package
  cmake-ts-mode
  :ensure nil
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

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
    'normal intercept-mode-map (kbd "grn") 'eglot-rename)
  (evil-define-key
    'normal intercept-mode-map (kbd "gra") 'eglot-code-actions)
  (evil-define-key
    'normal intercept-mode-map (kbd "<leader>cf") 'eglot-format))
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
  (corfu-auto-delay 0.15)
  (corfu-auto-prefix 2)
  (corfu-popupinfo-delay 0.25)
  :init
  (global-corfu-mode)
  (let ((inhibit-message t))
    (corfu-popupinfo-mode))
  :bind (:map corfu-map ("C-y" . corfu-complete)))
(use-package cape
  :ensure t
  :custom
  (cape-dabbrev-check-other-buffers nil)
  :config
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block))
