; Init GC

(setq gc-cons-threshold (* 50 1000 1000))

; Suppress package install noise

(add-to-list
 'display-buffer-alist
 '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
   (display-buffer-no-window)
   (allow-no-window . t)))

; Packages

(require 'package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)

; Theme

(use-package
 kanagawa-themes
 :config (load-theme 'kanagawa-wave :no-confirm-loading))

; Basic configuration

(use-package
 emacs
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

 (display-line-numbers-type 'relative)
 (global-display-line-numbers-mode t)
 (global-visual-line-mode t)

 (mouse-wheel-progressive-speed nil)
 (scroll-conservatively 10)
 (scroll-margin 8)

 (tab-width 4)
 (sgml-basic-offset 4)

 (use-dialog-box nil)
 (confirm-kill-processes nil)
 (set-buffer-modified-p nil)

 (make-backup-files nil)
 (auto-save-default nil)
 :hook (prog-mode . (lambda () (hs-minor-mode t)))
 :config
 (setq custom-file (locate-user-emacs-file "custom-vars.el"))
 (load custom-file 'noerror 'nomessage)
 :bind
 (([escape] . keyboard-escape-quit) ;; Makes Escape quit prompts (Minibuffer Escape)
  ("C-g" . evil-normal-state)))

; Easy scaling

(use-package
 emacs
 :bind
 ("C-=" . text-scale-increase)
 ("C--" . text-scale-decrease)
 ("<C-wheel-up>" . text-scale-increase)
 ("<C-wheel-down>" . text-scale-decrease))

; undo-tree

(use-package
 undo-tree
 :demand
 :custom (undo-tree-mode-lighter "")
 :custom
 (undo-tree-history-directory-alist
  `((".*" . ,(expand-file-name "undo-tree/" user-emacs-directory))))
 :config (global-undo-tree-mode))

; Set fonts

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

; Setup evil-mode

(use-package
 evil
 :init (evil-mode)
 :custom
 (evil-undo-system 'undo-tree)
 (evil-ex-previous-command nil)
 (evil-want-empty-ex-last-command t)
 (evil-want-C-u-scroll t)
 (evil-want-integration t)
 (evil-want-keybinding nil))
(use-package
 evil-collection
 :after evil
 :config
 ; (setq evil-collection-mode-list '(dired ibuffer magit corfu vertico consult vterm))
 (setq evil-collection-mode-list '(magit vertico consult))
 (evil-collection-init))
(use-package
 evil-commentary
 :after evil
 :config (evil-commentary-mode))

; magit

(use-package
 magit
 :custom (magit-diff-refine-hunk (quote all))
 :commands magit-status)

; .editorconfig

(use-package editorconfig :ensure t :config (editorconfig-mode 1))

; Hightlight on copy

(defun meain/evil-yank-advice (orig-fn beg end &rest args)
  (pulse-momentary-highlight-region beg end)
  (apply orig-fn beg end args))
(advice-add 'evil-yank :around 'meain/evil-yank-advice)

; Disable startup message

(fset 'display-startup-echo-area-message 'ignore)

; Fixed size line number column and relative numbers
(setq
 display-line-numbers-grow-only t
 display-line-numbers-type 'relative
 display-line-numbers-width-start t)
(global-display-line-numbers-mode)

; No weird gap on the left

(set-fringe-mode 0)

; No softwrap

(set-default 'truncate-lines t)

; Save history

(savehist-mode 1)

; LSP

(use-package
 eglot
 :ensure nil
 :hook
 ((c-ts-mode
   c++-ts-mode
   csharp-mode
   java-ts-mode
   html-mode
   css-ts-mode
   js-ts-mode
   typescript-ts-mode
   php-mode
   cmake-ts-mode
   go-mode
   rust-ts-mode
   gdscript-mode
   glsl-mode)
  . eglot-ensure)
 :custom
 (eglot-ignored-server-capabilities '(:inlayHintProvider))
 (eglot-events-buffer-size 0)
 (eglot-autoshutdown t)
 (eglot-report-progress nil)
 (eglot-stay-out-of '(flymake eldoc)))
; :config
; (add-to-list 'eglot-server-programs
;              `(cmake-ts-mode . ("~/.local/bin/cmake-language-server")))
; (add-to-list 'eglot-server-programs
;              `(glsl-mode . ("~/.config/emacs/lsp-servers/glsl_analyzer/glsl_analyzer"))))

(defvar my-intercept-mode-map (make-sparse-keymap)
  "High precedence keymap.")

(define-minor-mode my-intercept-mode
  "Global minor mode for higher precedence evil keybindings."
  :global t)

(my-intercept-mode)

(dolist (state '(normal))
  (evil-make-intercept-map
   (evil-get-auxiliary-keymap my-intercept-mode-map state t t) state))

(evil-define-key
 'normal my-intercept-mode-map (kbd "grn") 'eglot-rename)
(evil-define-key
 'normal my-intercept-mode-map (kbd "gra") 'eglot-code-actions)
(evil-define-key
 'normal my-intercept-mode-map (kbd "<leader>cf") 'eglot-format)

; Treesitter

(use-package
 treesit
 :ensure nil
 :config (setq treesit-font-lock-level 4))

(use-package
 treesit-auto
 :custom
 (treesit-auto-install 't)
 (c-ts-mode-indent-offset 4)
 (treesit-auto-langs '(c cpp glsl cmake))
 :config
 ;; Remove treesitter modes, go-ts-mode not working currently
 ;; glsl-ts-mode don't work because of a rewrite in glsl-mode
 ;; https://github.com/jimhourihan/glsl-mode/commit/c5f2c2e7edf8a647eda74abe2cdf73fa6f62ebd2
 (setq treesit-auto-langs
       (cl-set-difference
        treesit-auto-langs '(go gomod glsl c-sharp)))
 (treesit-auto-add-to-auto-mode-alist 'all)
 (global-treesit-auto-mode))

(use-package
 cmake-ts-mode
 :ensure nil
 :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

(use-package glsl-mode :mode ("\\.shader\\'" "\\.glsl\\'"))

; Vertico

(use-package
 vertico
 :config
 (setq vertico-cycle t)
 (setq vertico-resize nil)
 (vertico-mode 1))

(use-package
 emacs
 :custom (enable-recursive-minibuffers t)
 (read-extended-command-predicate
  #'command-completion-default-include-p)
 (minibuffer-prompt-properties
  '(read-only t cursor-intangible t face minibuffer-prompt)))

; Orderless

(use-package
 orderless
 :custom (completion-styles '(orderless basic))
 (completion-category-overrides
  '((file (styles basic partial-completion)))))

; Consult

(use-package consult)

(define-key evil-normal-state-map (kbd "<leader>sg") 'consult-ripgrep)
(define-key evil-normal-state-map (kbd "<leader>sf") 'consult-fd)

; Affe

(use-package affe :ensure t)

(defun affe-orderless-regexp-compiler (input _type _ignorecase)
  (setq input (cdr (orderless-compile input)))
  (cons input (apply-partially #'orderless--highlight input t)))
(setq affe-regexp-compiler #'affe-orderless-regexp-compiler)

; Format elisp

(use-package
 elisp-autofmt
 :commands (elisp-autofmt-mode elisp-autofmt-buffer)
 :hook (emacs-lisp-mode . elisp-autofmt-mode))

; Mappings

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

(define-key evil-normal-state-map (kbd "<leader>w") 'evil-write)
(define-key evil-normal-state-map (kbd "<leader>a") 'evil-write-all)
(define-key
 evil-normal-state-map (kbd "<leader>d") 'evil-delete-buffer)
(define-key
 evil-normal-state-map (kbd "<leader>q") 'evil-window-delete)

; Misc

(fset 'yes-or-no-p 'y-or-n-p)

; Runtime GC

(setq gc-cons-threshold (* 2 1000 1000))
(setq read-process-output-max (* 1024 1024))
