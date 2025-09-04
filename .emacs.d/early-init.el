;;; early-init.el --- Early Init -*- lexical-binding: t; -*-

;; Copyright (C) 2025 TideS

;; Author: TideS <tidesmain@gmail.com>

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

;;; Notes:

;;
;; Using local package repo with Elpaca:
;;
;; (elpaca '(package-name :repo "~/Projects/package-name")
;;   :init
;;   ...)
;;

;;; Code:

(defun suppress-messages (func &rest args)
  (cl-flet ((silence (&rest args1) (ignore)))
    (advice-add 'message :around #'silence)
    (unwind-protect
        (apply func args)
      (advice-remove 'message #'silence))))

;;; Optimize GC

(setq gc-cons-threshold (eval-when-compile (* 1024 1024 1024)))
(run-with-idle-timer 2 t 'garbage-collect)

;;; Maximize On Launch

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;; Lisp Settings

(add-to-list 'load-path (concat user-emacs-directory "lisp"))
(setq load-prefer-newer t)

;;; Elpaca

(setq package-enable-at-startup nil)
(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))
(elpaca elpaca-use-package
  (elpaca-use-package-mode)
  (setq elpaca-use-package-by-default t))
(elpaca-wait)

;;; Fonts

(let ((mono-spaced-font "Sarasa Term Slab CL")
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

;;; Themes

(use-package doric-themes
  :ensure (doric-themes
           :host github
           :repo "protesilaos/doric-themes"
           :inherit nil))

(use-package modus-themes
  :ensure (modus-themes
           :host github
           :repo "protesilaos/modus-themes"
           :inherit nil)
  :config
  (defun better-modus-faces (&rest _)
    (when-let* ((current-theme (car custom-enabled-themes))
                (modus-theme (member current-theme modus-themes-items)))
      (modus-themes-with-colors
        (custom-theme-set-faces
         current-theme
         `(eglot-mode-line
           ((,c :inherit mode-line-buffer-id :weight normal)))
         `(eglot-diagnostic-tag-unnecessary-face
           ((,c :inherit font-lock-comment-face)))
         `(region ((,c :background ,bg-region
                       :extend nil)))
         `(font-lock-keyword-face ((,c :inherit modus-themes-bold
                                       :foreground ,keyword
                                       :slant italic)))))))
  (add-hook 'enable-theme-functions #'better-modus-faces)
  (setopt modus-themes-italic-constructs t)
  (setopt modus-vivendi-tinted-palette-overrides '((comment fg-dim)))
  (setopt modus-themes-common-palette-overrides
          '((property fg-alt)
            (bracket fg-alt)
            (delimiter cyan-faint)
            (punctuation fg-alt)
            (variable fg-main)
            (operator cyan-faint)
            (number red-faint)
            (bg-active-argument unspecified)
            (bg-active-value unspecified)
            (fringe unspecified)
            (fg-line-number-inactive "gray50")
            (fg-line-number-active fg-main)
            (bg-line-number-inactive unspecified)
            (bg-line-number-active unspecified)))
  (dolist (theme modus-themes-items)
    (load-theme theme t t)
    (enable-theme theme))
  (enable-theme 'modus-vivendi))

;;; minions

(use-package minions
  :ensure t
  :config
  (minions-mode t))

;;; emacs

(use-package emacs
  :ensure nil
  :custom
  ;; (fringe-mode '(nil . 0))
  ;; (fringe-mode 0)
  (fringes-outside-margins t)
  (isearch-lazy-count t)
  (isearch-lazy-highlight t)
  (vc-follow-symlinks t)
  (native-comp-jit-compilation-deny-list '(".*-loaddefs.el.gz"))
  (delete-by-moving-to-trash t)
  (use-short-answers t)
  (frame-inhibit-implied-resize t)
  (frame-resize-pixelwise t)
  (auto-mode-case-fold nil)
  (warning-suppress-log-types '((native-compiler)))
  (ring-bell-function 'ignore)
  (help-window-select t)
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
  (mouse-wheel-progressive-speed nil)
  ;; (scroll-error-top-bottom t)
  ;; (scroll-margin 6)
  ;; (scroll-conservatively 101)
  ;; (scroll-up-aggressively 0.01)
  ;; (scroll-down-aggressively 0.01)
  (scroll-preserve-screen-position t)
  ;; (auto-window-vscroll nil)
  ;; (fast-but-imprecise-scrolling t)
  (fill-column 80)
  (tab-always-indent t)
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
  (switch-to-buffer-obey-display-actions t)
  ;; (pixel-scroll-precision-mode t)
  ;; (pixel-scroll-precision-use-momentum nil)
  (bidi-display-reordering 'left-to-right)
  (bidi-paragraph-direction 'left-to-right)
  (bidi-inhibit-bpa t)
  (initial-major-mode 'text-mode)
  (redisplay-skip-fontification-on-input t)
  (window-divider-default-bottom-width 1)
  (window-divider-default-places t)
  (window-divider-default-right-width 1)
  (mouse-yank-at-point nil)
  (indicate-buffer-boundaries nil)
  (indicate-empty-lines nil)
  (compilation-scroll-output t)
  (mouse-highlight nil)
  (show-help-function nil)
  (tooltip-mode nil)
  (blink-cursor-blinks 0)
  (blink-cursor-mode t)
  :init
  (advice-add 'recentf-cleanup :around #'suppress-messages)
  (advice-add 'recentf-mode :around #'suppress-messages)
  (advice-add 'display-startup-echo-area-message :override #'ignore)
  (advice-add 'display-startup-screen :override #'ignore)
  :config
  (let ((inhibit-message t))
    (recentf-mode t)
    (global-auto-revert-mode t)
    (savehist-mode t)
    (electric-indent-mode t)
    (electric-pair-mode t)
    (delete-selection-mode t))

  (add-hook 'display-line-numbers-mode-hook
            (lambda ()
              (setq left-margin-width
                    (if display-line-numbers-mode 1 0))
              (set-window-buffer (selected-window)
                                 (window-buffer (selected-window)))))

  (modify-coding-system-alist 'file "" 'utf-8)

  (setopt custom-file (locate-user-emacs-file "custom.el"))
  (load custom-file 'noerror 'nomessage))

;;; ansi-color

(use-package ansi-color
  :ensure nil
  :hook (compilation-filter . ansi-color-compilation-filter))

;;; Popper

(use-package popper
  :ensure t
  :after evil
  :config
  (evil-define-key 'normal 'global (kbd "<localleader>x") 'popper-toggle)
  (evil-define-key 'normal 'global (kbd "<leader>x") 'popper-toggle)
  (setopt popper-mode-line "")
  (setopt popper-display-control t)
  ;; (setq popper-display-function #'display-buffer-pop-up-window)
  (setopt popper-reference-buffers
          '(
            ;; "^\\*.*\\*$"
            "\\*eldoc\\*"
            "^\\*godot"
            "\\*Messages\\*"
            "\\*Warnings\\*"
            "Output\\*$"
            "\\*Async Shell Command\\*"
            xref--xref-buffer-mode
            help-mode
            compilation-mode))
  (setopt popper-window-height (lambda (win)
                                 (fit-window-to-buffer
                                  win
                                  (* 2 (floor (frame-height) 5))
                                  (floor (frame-height) 3))))
  (popper-mode t))

;;; dired

(use-package dired
  :ensure nil
  :custom
  (dired-auto-revert-buffer t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-ls-F-marks-symlinks t)
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-listing-switches "-lah --group-directories-first")
  (dired-dwim-target t)
  :config
  (add-hook 'dired-mode-hook
            (lambda ()
              ;; (dired-hide-details-mode 1)
              (hl-line-mode 1))))

(use-package diredfl
  :ensure t
  :config
  (diredfl-global-mode))

;;; eldoc

(use-package eldoc
  :ensure nil
  :custom
  (eldoc-echo-area-use-multiline-p nil)
  ;; (eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
  (eldoc-idle-delay 0.1)
  (eldoc-print-after-edit nil)
  (eldoc-echo-area-display-truncation-message nil))

;;; org

(use-package org
  :ensure nil
  :hook (org-mode . (lambda () (setq-local truncate-lines nil))))

;;; undo-fu

(use-package undo-fu
  :ensure t)

(use-package undo-fu-session
  :ensure t
  :hook (after-init . undo-fu-session-global-mode))

;;; evil-mode

(use-package evil
  :ensure t
  :preface
  (setopt evil-want-empty-ex-last-command nil)
  (setopt evil-want-keybinding nil)
  (setopt evil-want-C-u-scroll t)
  (setopt evil-want-C-d-scroll t)
  (setopt evil-want-Y-yank-to-eol t)
  (setopt evil-want-integration t)
  (setopt evil-want-fine-undo t)
  (setopt evil-want-minibuffer t)
  (setopt evil-leader/in-all-states t)
  (setopt evil-undo-system 'undo-fu)
  (setopt evil-vsplit-window-right t)
  (setopt evil-auto-balance-windows nil)
  (setopt evil-echo-state nil)
  (setopt evil-kill-on-visual-paste nil)
  (setopt evil-search-module 'evil-search)
  (setopt evil-insert-state-cursor nil)
  :config
  (evil-mode t)
  (evil-set-leader 'normal (kbd "SPC"))
  (evil-set-leader 'normal "\\" t)

  ;; (define-key evil-command-line-map [tab] nil t)
  (evil-define-key 'insert 'global (kbd "\C-y") nil)
  (evil-define-key 'normal 'global (kbd "C-l") 'evil-ex-nohighlight)
  (evil-define-key 'insert 'global (kbd "TAB") 'tab-to-tab-stop)
  (evil-define-key '(normal motion visual)
    'global (kbd "H") 'evil-first-non-blank)
  (evil-define-key '(normal motion visual)
    'global (kbd "L") 'evil-end-of-line)
  (evil-define-key 'normal 'global (kbd "<leader>w") 'evil-write)
  (evil-define-key 'normal 'global (kbd "<leader>a") 'evil-write-all)
  (evil-define-key 'normal 'global (kbd "<leader>d") 'kill-current-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>q") 'evil-window-delete)

  (evil-define-key 'normal 'global
    (kbd "<leader>tn") 'display-line-numbers-mode)
  (evil-define-key 'normal 'global (kbd "<leader>td") 'global-diff-hl-mode)
  (evil-define-key 'normal 'global (kbd "<leader>tl") 'global-hl-line-mode)
  (evil-define-key 'normal 'global (kbd "<leader>to") 'ff-find-other-file)

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
  (define-minor-mode intercept-mode
    "High precedence keymap for evil."
    :global t)
  (intercept-mode)
  (dolist (state '(normal))
    (evil-make-intercept-map
     (evil-get-auxiliary-keymap
      intercept-mode-map state t t)
     state))

  (defun scroll-to-center-after-goto-mark (char &optional noerror) (recenter))
  (evil--advice-add 'evil-goto-mark :after #'scroll-to-center-after-goto-mark)

  (defun evil-yank-highlight (orig-fn beg end &rest args)
    (pulse-momentary-highlight-region beg end)
    (apply orig-fn beg end args))
  (advice-add 'evil-yank :around 'evil-yank-highlight))

(use-package
  evil-collection
  :ensure t
  :after evil
  :preface
  (setopt evil-collection-magit-section-use-z-for-folds t)
  (setopt evil-collection-setup-minibuffer t)
  (setopt evil-collection-want-find-usages-bindings t)
  :config
  (evil-collection-init))

(use-package
  evil-commentary
  :ensure t
  :config (evil-commentary-mode))

(elpaca-wait)

;;; drag-stuff

(use-package drag-stuff
  :ensure t
  :config
  (evil-define-key 'visual 'global (kbd "J") 'drag-stuff-down)
  (evil-define-key 'visual 'global (kbd "K") 'drag-stuff-up))

;;; Apheleia

(use-package apheleia
  :ensure t)

;;; magit

(use-package transient
  :ensure t)
(use-package diff-hl
  :ensure t)
(use-package magit
  :ensure t
  :after (transient diff-hl)
  :custom
  (magit-section-visibility-indicator nil))

;;; hl-todo

(use-package hl-todo
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'global-hl-todo-mode))

;;; flymake

(use-package flymake
  :ensure t
  :custom
  (flymake-margin-indicators-string
   '((error "E" compilation-error)
     (warning "W" compilation-warning)
     (note "I" compilation-info)))
  (flymake-indicator-type 'margins)
  (flymake-autoresize-margins nil)
  ;; (flymake-show-diagnostics-at-end-of-line 'fancy)
  ;; (flymake-indicator-type nil)
  ;; (flymake-margin-indicator-position 'right-margin)
  ;; (flymake-fringe-indicator-position nil)
  :bind
  (:map evil-normal-state-map
        ("[d" . flymake-goto-prev-error)
        ("]d" . flymake-goto-next-error)))

;;; EditorConfig

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;;; Markdown

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode))

;;; CMake

(use-package cemako
  :ensure nil
  :bind
  ("<leader>bt" . cemako-select-target)
  ("<leader>bc" . cemako-run-cmake)
  ("<leader>bb" . cemako-build)
  ("<leader>br" . cemako-run))

;;; GLSL

(use-package glsl-mode
  :ensure (glsl-mode
           :host github
           :repo "TideSofDarK/glsl-mode"
           :branch "better-ts-mode"
           :inherit nil)
  :init
  (custom-set-faces
   '(glsl-extension-face
     ((t :inherit font-lock-constant-face))))
  (custom-set-faces
   '(glsl-shader-variable-name-face
     ((t :inherit font-lock-preprocessor-face :slant italic)))))

;;; Godot

(use-package gdscript-mode
  :ensure (gdscript-mode
           :host github
           :repo "godotengine/emacs-gdscript-mode"
           :inherit nil)
  :custom
  (gdscript-eglot-version "4.4")
  (gdscript-indent-offset 4)
  (gdscript-use-tab-indents nil))

(use-package gdshader-mode
  :ensure (gdshader-mode
           :host github
           :repo "bbbscarter/gdshader-mode"
           :inherit nil)
  :after cape
  :init
  (defvar gdshader-keyword-list
    '("shader_type" "render_mode" "group_uniforms" "instance" "varying"))
  (defun gdshader-config()
    (setq-local completion-at-point-functions
                (list (cape-capf-super #'cape-dabbrev #'cape-keyword))))
  :hook (gdshader-mode . gdshader-config)
  :config
  (with-eval-after-load 'cape-keyword
    (add-to-list
     'cape-keyword-list
     (append '(gdshader-mode) gdshader-all-keywords))))

;;; Treesitter

(use-package treesit
  :ensure nil
  :custom
  (treesit-font-lock-level 4))

(use-package treesit-langs
  :ensure (treesit-langs
           :host github
           :repo "emacs-tree-sitter/treesit-langs"
           :inherit nil
           :after treesit)
  :custom
  (treesit-langs-bundle-version "0.12.301")
  :init
  (advice-add 'treesit-langs-install-grammars :around #'suppress-messages)
  :config
  (treesit-langs-major-mode-setup)
  (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
  (use-package glsl-ts-mode :ensure nil)
  (use-package cmake-ts-mode :ensure nil)
  (use-package rust-ts-mode :ensure nil)
  (use-package lua-ts-mode :ensure nil))

(use-package c-ts-mode-extras
  :ensure (c-ts-mode-extras
           :host github
           :repo "TideSofDarK/c-ts-mode-extras"
           :inherit nil
           :after treesit-langs)
  :config
  ;; (defun custom-c-ts-mode-indent-style()
  ;;   `(
  ;;     ((node-is "preproc") column-0 0)
  ;;     ((n-p-gp nil "declaration_list" "namespace_definition") parent-bol 0)
  ;;     ((node-is ")") parent-bol 0)
  ;;     ((match nil "argument_list" nil 1 1) parent-bol c-ts-mode-indent-offset)
  ;;     ((parent-is "argument_list") prev-sibling 0)
  ;;     ((match nil "parameter_list" nil 1 1) parent-bol c-ts-mode-indent-offset)
  ;;     ((parent-is "parameter_list") prev-sibling 0)
  ;;     ,@(alist-get 'bsd (c-ts-mode--indent-styles 'cpp))))
  (add-hook 'c-ts-mode-hook
            (lambda ()
              (electric-indent-local-mode -1)))
  (add-hook 'c++-ts-mode-hook
            (lambda ()
              (electric-indent-local-mode -1)))
  (setopt c-ts-mode-indent-offset 4)
  (setopt c-ts-mode-indent-style 'bsd)
  (setopt c-ts-mode-enable-doxygen t))

;;; Eglot

(use-package eglot
  :ensure t
  :hook
  ((c-ts-mode c++-ts-mode gdscript-ts-mode
              glsl-ts-mode cmake-ts-mode)
   . eglot-ensure)
  :custom
  (eglot-mode-line-format
   '(eglot-mode-line-menu
     eglot-mode-line-session
     eglot-mode-line-action-suggestion))
  (eglot-ignored-server-capabilities
   '(:inlayHintProvider
     :documentHighlightProvider))
  (eglot-events-buffer-size 0)
  (eglot-autoshutdown t)
  (eglot-report-progress nil)
  (eglot-events-buffer-config '(:size 0 :format full))
  (eglot-send-changes-idle-time 0.1)
  (eglot-extend-to-xref t)
  (eglot-code-action-indications '(eldoc-hint))
  ;; (eldoc-idle-delay 0.1)
  :config
  (fset #'jsonrpc--log-event #'ignore)
  (add-to-list 'eglot-server-programs
               '((c-ts-mode c++-ts-mode)
                 . ("clangd"
                    "-j=8"
                    "--log=error"
                    "--background-index"
                    "--clang-tidy"
                    "--completion-style=detailed"
                    "--pch-storage=memory"
                    "--header-insertion=never"
                    "--header-insertion-decorators=0")))
  (add-to-list 'eglot-server-programs
               `(glsl-ts-mode . ("glsl_analyzer")))
  (add-to-list 'eglot-server-programs
               `(cmake-ts-mode . ("cmake-language-server")))
  (evil-define-key
    'normal intercept-mode-map (kbd "grn") 'eglot-rename)
  (evil-define-key
    'normal intercept-mode-map (kbd "gra") 'eglot-code-actions)
  (evil-define-key
    'normal eglot-mode-map (kbd "<leader>cf") 'eglot-format))

(use-package eglot-inactive-regions
  :ensure t
  :custom
  (eglot-inactive-regions-style 'shadow-face)
  :config
  (eglot-inactive-regions-mode 1))

;;; Completion

(use-package consult
  :ensure t
  :custom
  (consult-line-start-from-top t)
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  :config
  (advice-add #'register-preview :override #'consult-register-window)
  (evil-define-key 'normal intercept-mode-map (kbd "grr") 'xref-find-references)
  (evil-define-key 'normal 'global (kbd "gO") 'consult-imenu)
  (evil-define-key 'normal 'global (kbd "<leader>/") 'consult-line)
  (evil-define-key 'normal 'global (kbd "<leader>st") 'consult-theme)
  (evil-define-key 'normal 'global (kbd "<leader>sg") 'consult-ripgrep)
  (evil-define-key 'normal 'global (kbd "<leader>sf") 'project-find-file)
  (evil-define-key 'normal 'global (kbd "<leader>SPC") 'consult-buffer))

(use-package consult-eglot
  :ensure t
  :config
  (evil-define-key 'normal 'global (kbd "gW") 'consult-eglot-symbols))

(use-package vertico
  :ensure t
  :custom
  (vertico-cycle t)
  (vertico-resize nil)
  :config
  (keymap-set vertico-map "C-y" #'vertico-insert)
  (vertico-mode 1))

(use-package vertico-repeat
  :ensure nil
  :after vertico
  :config
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  (evil-define-key 'normal 'global (kbd "<leader>sr") 'vertico-repeat))

(use-package
  orderless
  :ensure t
  :custom
  (completion-styles '(orderless flex))
  (completion-category-defaults nil)
  (completion-category-overrides
   '((file (styles basic partial-completion))
     (imenu (styles flex))
     (eglot (styles orderless))
     (eglot-capf (styles orderless)))))

(use-package marginalia
  :ensure t
  :init (marginalia-mode))

(use-package corfu
  :ensure t
  :custom
  (corfu-left-margin-width 0.0)
  (corfu-right-margin-width 0.0)
  (corfu-bar-width 0.0)
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 2)
  (corfu-popupinfo-delay 0.25)
  :config
  (global-corfu-mode)
  (let ((inhibit-message t))
    (corfu-popupinfo-mode))
  (keymap-unset corfu-map "RET")
  (keymap-set corfu-map "C-y" #'corfu-complete))

(use-package cape
  :ensure t
  :custom
  (cape-dabbrev-check-other-buffers nil)
  :config
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block))

;;; Done

(provide 'early-init)
;;; early-init.el ends here
