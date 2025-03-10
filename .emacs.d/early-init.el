; Init GC

(setq gc-cons-threshold (* 50 1000 1000))

; Add lisp path

(add-to-list 'load-path (concat user-emacs-directory "lisp"))

; Packages

(setq package-enable-at-startup nil)
(defvar elpaca-installer-version 0.10)
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
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))
(elpaca elpaca-use-package
  (elpaca-use-package-mode))

; Set font

(let ((mono-spaced-font "Sarasa Fixed CL Nerd Font")
      (proportionately-spaced-font "Sarasa UI CL Nerd Font"))
  (set-face-attribute 'default nil
                      :family mono-spaced-font
                      :height 120)
  (set-face-attribute 'fixed-pitch nil
                      :family mono-spaced-font
                      :height 1.0)
  (set-face-attribute 'variable-pitch nil
                      :family proportionately-spaced-font
                      :height 1.0))

; Themes

(use-package autothemer
 :ensure t
 :init
 (require 'kanagawa-themes)
 (require 'rose-pine-theme)
 (load-theme 'kanagawa-wave :no-confirm-loading))

; Basic configuration

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
 (read-extended-command-predicate #'command-completion-default-include-p)
 (minibuffer-prompt-properties
  '(read-only t cursor-intangible t face minibuffer-prompt))

 :hook (prog-mode . (lambda () (hs-minor-mode t)))
 :config
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
 ;  ("C-g" . evil-normal-state)))

; undo-tree

(use-package
 undo-tree
 :ensure t
 :demand t
 :custom (undo-tree-mode-lighter "")
 :custom
 (undo-tree-history-directory-alist
  `((".*" . ,(expand-file-name "undo-tree/" user-emacs-directory))))
 :config (global-undo-tree-mode))

; Setup evil-mode

(use-package
 evil
 :ensure t
 :init (evil-mode)
 :custom
 (evil-undo-system 'undo-tree)
 (evil-ex-previous-command nil)
 (evil-want-empty-ex-last-command t)
 (evil-want-C-u-scroll t)
 (evil-want-integration t)
 (evil-want-keybinding nil)
 :config
 (evil-set-leader 'normal (kbd "SPC"))
 (evil-set-leader 'normal "\\" t)

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

 (define-key evil-normal-state-map (kbd "<leader>tn") 'global-display-line-numbers-mode)

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
    (evil-get-auxiliary-keymap my-intercept-mode-map state t t) state)))
(use-package
 evil-collection
 :ensure t
 :after evil
 :config
 ; (setq evil-collection-mode-list '(dired ibuffer magit corfu vertico consult vterm))
 (setq evil-collection-mode-list '(dired ibuffer magit vertico consult eldoc eglot corfu))
 (evil-collection-init))
(use-package
 evil-commentary
 :ensure t
 :after evil
 :config (evil-commentary-mode))

(defun meain/evil-yank-advice (orig-fn beg end &rest args)
  (pulse-momentary-highlight-region beg end)
  (apply orig-fn beg end args))
(advice-add 'evil-yank :around 'meain/evil-yank-advice)

; magit

(elpaca transient)
(elpaca (magit :wait t))

; EditorConfig

(use-package editorconfig
 :ensure t
 :config (editorconfig-mode 1))

; LSP

(use-package
 eglot
 :ensure nil
 :hook
 ((c-ts-mode
   c++-ts-mode
   typescript-ts-mode
   ; cmake-ts-mode
   rust-ts-mode
   ; glsl-mode
   gdscript-mode)
  . eglot-ensure)
 :custom
 (eglot-ignored-server-capabilities '(:inlayHintProvider :documentHighlightProvider))
 (eglot-events-buffer-size 0)
 (eglot-autoshutdown t)
 (eglot-report-progress nil)
 (eglot-stay-out-of '(flymake eldoc))
 :config
; (add-to-list 'eglot-server-programs
;              `(cmake-ts-mode . ("~/.local/bin/cmake-language-server")))
; (add-to-list 'eglot-server-programs
;              `(glsl-mode . ("~/.config/emacs/lsp-servers/glsl_analyzer/glsl_analyzer"))))
 (evil-define-key
  'normal my-intercept-mode-map (kbd "grn") 'eglot-rename)
 (evil-define-key
  'normal my-intercept-mode-map (kbd "gra") 'eglot-code-actions)
 (evil-define-key
  'normal my-intercept-mode-map (kbd "<leader>cf") 'eglot-format))
(use-package eldoc-box
  :ensure t
  :config
 (evil-define-key
  'normal my-intercept-mode-map (kbd "K") 'eldoc-box-help-at-point))
  ;; (defun eldoc-box-scroll-up ()
  ;;   "Scroll up in `eldoc-box--frame'"
  ;;   (interactive)
  ;;   (with-current-buffer eldoc-box--buffer
  ;;     (with-selected-frame eldoc-box--frame
  ;;       (scroll-down 3))))
  ;; (defun eldoc-box-scroll-down ()
  ;;   "Scroll down in `eldoc-box--frame'"
  ;;   (interactive)
  ;;   (with-current-buffer eldoc-box--buffer
  ;;     (with-selected-frame eldoc-box--frame
  ;;       (scroll-up 3))))
  ;; (:keymaps 'eglot-mode-map
  ;;  :states '(insert normal hybrid motion visual operator emacs)
  ;;  "C-k" 'eldoc-box-scroll-up
  ;;  "C-j" 'eldoc-box-scroll-down
  ;;  "K" 'eldoc-box-eglot-help-at-point))

; Treesitter

(use-package
 treesit
 :ensure nil
 :config (setq treesit-font-lock-level 4))
(use-package
 treesit-auto
 :ensure t
 :demand t
 :custom
 (treesit-auto-install 't)
 :config
 (setq treesit-auto-langs '(c cpp cmake toml yaml commonlisp markdown))
 (treesit-auto-install-all)
 (global-treesit-auto-mode))
(use-package
 cmake-ts-mode
 :ensure nil
 :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

; Markdown

(use-package markdown-mode
 :ensure t
 :mode ("README\\.md\\'" . gfm-mode)
 :init (setq markdown-command "multimarkdown")
 :bind (:map markdown-mode-map
        ("C-c C-e" . markdown-do)))

; GLSL

(use-package
 glsl-mode
 :ensure t
 :mode ("\\.shader\\'" "\\.glsl\\'"))

; Godot

(elpaca
 (gdscript-mode
  :host github
  :repo "godotengine/emacs-gdscript-mode"
  :inherit nil))

; Completion

(use-package
 consult
 :ensure t
 :config
 (define-key evil-normal-state-map (kbd "<leader>SPC") 'consult-buffer))
(use-package
 vertico
 :ensure t
 :config
 (setq vertico-cycle t)
 (setq vertico-resize nil)
 (vertico-mode 1))
(use-package
 orderless
 :ensure t
 :custom
 ;; (orderless-style-dispatchers '(orderless-affix-dispatch))
 ;; (orderless-component-separator #'orderless-escapable-split-on-space)
 (completion-styles '(orderless basic))
 (completion-category-defaults nil)
 (completion-category-overrides '(
  (file (styles basic partial-completion))
  (eglot (styles orderless)))))
(use-package marginalia
 :ensure t
 :init
 (marginalia-mode))
(use-package affe
 :ensure t
 :config
 (define-key evil-normal-state-map (kbd "<leader>sg") 'affe-grep)
 (define-key evil-normal-state-map (kbd "<leader>sf") 'affe-find)
 (setq affe-find-command "fd --color=never --full-path")
 (defun affe-orderless-regexp-compiler (input _type _ignorecase)
  (setq input (cdr (orderless-compile input)))
  (cons input (apply-partially #'orderless--highlight input t)))
 (setq affe-regexp-compiler #'affe-orderless-regexp-compiler)
 (consult-customize affe-grep :preview-key "M-."))
(use-package corfu
 :ensure t
 :custom
 (corfu-cycle t)
 (corfu-quit-at-boundary nil)
 (corfu-auto t)
 (corfu-quit-no-match 'separator)
 (corfu-preview-current 'insert)
 (corfu-preselect-first t)
 :bind (:map corfu-map
           ("C-h" . corfu-info-documentation)
           ("C-y" . corfu-complete)
           ("RET" . nil)
           ("<return>" . nil))
 :init
 (corfu-popupinfo-mode)
 (global-corfu-mode))
(use-package emacs
  :ensure nil
  :custom
  (tab-always-indent 'complete)
  (text-mode-ispell-word-completion nil)
  (read-extended-command-predicate #'command-completion-default-include-p))

; Format elisp

(use-package
 elisp-autofmt
 :ensure t
 :commands (elisp-autofmt-mode elisp-autofmt-buffer)
 :hook (emacs-lisp-mode . elisp-autofmt-mode))

; Runtime GC

(setq gc-cons-threshold (* 2 1000 1000))
(setq read-process-output-max (* 1024 1024))
