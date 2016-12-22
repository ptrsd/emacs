;; global variables
(setq
 inhibit-startup-screen t
 create-lockfiles nil
 make-backup-files nil
 column-number-mode t
 scroll-error-top-bottom t
 show-paren-delay 0.5
 use-package-always-ensure t
 sentence-end-double-space nil)

;; buffer local variables
(setq-default
 indent-tabs-mode nil
 tab-width 4
 c-basic-offset 4)

(fset `yes-or-no-p `y-or-n-p)

;; modes
(electric-indent-mode 0)

;; global keybindings
(setq mac-command-modifier 'super)

;; the package manager
(require 'package)
(setq
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("org" . "http://orgmode.org/elpa/")
                    ("melpa" . "http://melpa.org/packages/")
                    ("melpa-stable" . "http://stable.melpa.org/packages/"))
 package-archive-priorities '(("melpa-stable" . 1)))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; ui
(use-package monokai-theme
  :init (load-theme 'monokai t))

(tool-bar-mode 0)
(menu-bar-mode 0)
(toggle-frame-fullscreen)
(scroll-bar-mode 0)

(use-package window-numbering
  :init (window-numbering-mode))

(custom-set-faces '(default ((t (:background "#333333")))))

;; evil
(use-package evil 
  :init (evil-mode 1))

(use-package evil-escape
  :diminish evil-escape-mode
  :config (evil-escape-mode))

;; ensime
(use-package ensime 
  :ensure t 
  :pin melpa-stable
  :bind ("M-RET" . ensime-edit-definition))

;; helm
(use-package helm) 

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-m") 'helm-M-x)
(global-set-key (kbd "C-c C-m") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

;; projectile
(use-package projectile
  :demand
  :init   (setq projectile-use-git-grep t)
  :config (projectile-global-mode t)
  :bind   (("M-f" . projectile-find-file)
           ("M-r" . helm-projectile-grep)))

(use-package helm-projectile
  :init (helm-projectile-on))

;; dashboard
(use-package dashboard
  :config
  (dashboard-setup-startup-hook))

;; undo
(use-package undo-tree
  :diminish undo-tree-mode
  :config (global-undo-tree-mode)
  :bind ("M-/" . undo-tree-visualize))

;; move-text
(use-package expand-region)
(use-package move-text)

;; neotree
 (use-package neotree
  :init (setq neo-smart-open t)
  :bind ("C-c n" . neotree-toggle))

;; elscreen
(use-package elscreen
  :init (elscreen-start)
  :config (setq elscreen-display-tab nil))

(define-key evil-normal-state-map "\C-zc" 'elscreen-create)
(define-key evil-normal-state-map "\C-zk" 'elscreen-kill)
(define-key evil-normal-state-map "gt" 'elscreen-next)
(define-key evil-normal-state-map "gT" 'elscreen-previous)

;; which-key
(use-package which-key
  :init (which-key-mode))

;; powerline
(use-package powerline
  :init (powerline-vim-theme))

;; windmove
(use-package windmove
  :init (windmove-default-keybindings))

;; rainbow-delimiters
(use-package rainbow-delimiters
  :init (rainbow-delimiters-mode)) 

;; git-gutter
(use-package git-gutter
  :init (global-git-gutter-mode +1))

;; smartparentheses
(use-package smartparens
  :diminish smartparens-mode
  :commands
  smartparens-strict-mode
  smartparens-mode
  sp-restrict-to-pairs-interactive
  sp-local-pair
  :init
  (setq sp-interactive-dwim t)
  :config
  (require 'smartparens-config)
  (sp-use-smartparens-bindings)

  (sp-pair "(" ")" :wrap "C-(")
  (sp-pair "[" "]" :wrap "s-[")
  (sp-pair "{" "}" :wrap "C-{")

(bind-key "C-<left>" nil smartparens-mode-map)
(bind-key "C-<right>" nil smartparens-mode-map)

(bind-key "s-<delete>" 'sp-kill-sexp smartparens-mode-map)
(bind-key "s-<backspace>" 'sp-backward-kill-sexp smartparens-mode-map))

;; completions
(use-package company)
(global-set-key (kbd "TAB") 'company-complete)

;; global keybindings
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "<home>") 'back-to-indentation)
(global-set-key (kbd "<end>") 'move-end-of-line)
(global-set-key (kbd "<S-up>") 'move-text-up)
(global-set-key (kbd "<S-down>") 'move-text-down)

(global-set-key [next]
  (lambda () (interactive)
    (condition-case nil (scroll-up)
      (end-of-buffer (goto-char (point-max))))))

(global-set-key [prior]
  (lambda () (interactive)
    (condition-case nil (scroll-down)
      (beginning-of-buffer (goto-char (point-min))))))

;; hooks
(add-hook 'prog-mode-hook 
  (lambda () 
    (global-company-mode)
    (rainbow-delimiters-mode)
    (smartparens-global-mode)))

