; Packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu-devel" . "https://elpa.gnu.org/devel/"))
(package-initialize)
(add-to-list 'display-buffer-alist
             '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
               (display-buffer-no-window)
               (allow-no-window . t)))

; Theme
(use-package kanagawa-themes
  :ensure t
  :config
  (load-theme 'kanagawa-wave :no-confirm-loading))

; Disable some GUI elements
(setq use-dialog-box nil)
(setq confirm-kill-processes nil)
(set-buffer-modified-p nil)
(tool-bar-mode     -1)
(scroll-bar-mode   -1)
(menu-bar-mode     -1)
(blink-cursor-mode -1)

; Default tab width
(setq-default indent-tabs-mode nil
              tab-width 4)

; Set fonts
(let ((mono-spaced-font "Sarasa Fixed CL Nerd Font")
      (proportionately-spaced-font "Sarasa UI CL Nerd Font"))
  (set-face-attribute 'default nil :family mono-spaced-font :height 120)
  (set-face-attribute 'fixed-pitch nil :family mono-spaced-font :height 1.0)
  (set-face-attribute 'variable-pitch nil :family proportionately-spaced-font :height 1.0))

; Setup evil-mode
(setq evil-want-C-u-scroll t)
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))
(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))
(use-package evil-commentary
  :after evil
  :ensure t
  :config
  (evil-commentary-mode))

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
(setq display-line-numbers-grow-only   t
      display-line-numbers-type        'relative
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

; Modeline
(use-package smart-mode-line
  :config
  (progn
   (setq sml/no-confirm-load-theme t)
   (sml/setup)))

; Mappings

(evil-set-leader 'normal (kbd "SPC"))
(evil-set-leader 'normal "\\" t)

(define-key evil-normal-state-map (kbd "M-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "M-l") 'evil-window-right)
(define-key evil-normal-state-map (kbd "M-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "M-j") 'evil-window-down)

(define-key evil-normal-state-map "H" 'evil-beginning-of-line)
(define-key evil-normal-state-map "L" 'evil-end-of-line)

(define-key evil-normal-state-map (kbd "<leader>w") 'evil-write)
(define-key evil-normal-state-map (kbd "<leader>a") 'evil-write-all)
