; Packages

(require 'package)
(add-to-list
 'package-archives '("melpa" . "https://melpa.org/packages/")
 t)
(add-to-list
 'package-archives '("gnu-devel" . "https://elpa.gnu.org/devel/"))
(package-initialize)
(add-to-list
 'display-buffer-alist
 '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
   (display-buffer-no-window)
   (allow-no-window . t)))

; Theme

(use-package
 kanagawa-themes
 :ensure t
 :config (load-theme 'kanagawa-wave :no-confirm-loading))

; Disable some GUI elements

(setq use-dialog-box nil)
(setq confirm-kill-processes nil)
(set-buffer-modified-p nil)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode -1)

; Default tab width

(setq-default
 indent-tabs-mode nil
 tab-width 4)

; .editorconfig

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

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

(setq evil-ex-previous-command nil)
(setq evil-want-empty-ex-last-command t)
(setq evil-want-C-u-scroll t)
(use-package
 evil
 :ensure t
 :init
 (setq evil-want-integration t)
 (setq evil-want-keybinding nil)
 :config (evil-mode 1))
(use-package
 evil-collection
 :after evil
 :ensure t
 :config (evil-collection-init))
(use-package
 evil-commentary
 :after evil
 :ensure t
 :config (evil-commentary-mode))

; Hightlight on copy

(defun meain/evil-yank-advice (orig-fn beg end &rest args)
  (pulse-momentary-highlight-region beg end)
  (apply orig-fn beg end args))
(advice-add 'evil-yank :around 'meain/evil-yank-advice)

; No default messages

(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(fset 'display-startup-echo-area-message 'ignore)

; Fixed size line number column and relative numbers
(setq
 display-line-numbers-grow-only t
 display-line-numbers-type 'relative
 display-line-numbers-width-start t)
(global-display-line-numbers-mode)

; No weird gap on the left

(set-fringe-mode 0)

; Scrolloffs

(setq scroll-conservatively 10)
(setq scroll-margin 7)

; No softwrap

(set-default 'truncate-lines t)

; Save history

(savehist-mode 1)

; Misc

(fset 'yes-or-no-p 'y-or-n-p)
(display-time)
(global-eldoc-mode -1)

; LSP

(use-package
 eglot
 :hook (prog-mode . eglot-ensure)
 :init (setq eglot-stay-out-of '(flymake eldoc)))

(setq eglot-ignored-server-capabilities '(:inlayHintProvider))
(setq eglot-report-progress nil)

; Treesitter

(use-package treesit :config (setq treesit-font-lock-level 4))

(use-package
 treesit-auto
 :ensure t
 :custom (treesit-auto-install 'prompt)
 :config
 (treesit-auto-add-to-auto-mode-alist 'all)
 (global-treesit-auto-mode))

; Vertico

(use-package vertico :ensure t :init (vertico-mode))

(use-package
 emacs
 :custom (enable-recursive-minibuffers t)
 (read-extended-command-predicate
  #'command-completion-default-include-p)
 (minibuffer-prompt-properties
  '(read-only t cursor-intangible t face minibuffer-prompt)))

; Format elisp

(use-package
 elisp-autofmt
 :ensure t
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

(define-key evil-normal-state-map (kbd "<leader>cf") 'eglot-format)

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
