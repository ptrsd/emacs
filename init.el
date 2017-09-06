;; global variables
(setq
 inhibit-startup-screen t
 create-lockfiles nil
 make-backup-files nil
 column-number-mode t
 scroll-error-top-bottom t
 make-backup-files nil
 show-paren-delay 0.5
 use-package-always-ensure t
 sentence-end-double-space nil)

;; buffer local variables
(setq-default
 indent-tabs-mode nil
 tab-width 4
 c-basic-offset 4)

(fset `yes-or-no-p `y-or-n-p)

(set-face-attribute 'default nil
                    :family "Hack"
                    :height 100
                    :weight 'normal
                    :width 'normal)

;; global
(setq mac-command-modifier 'super)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; the package manager
(require 'package)
(setq
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("org" . "http://orgmode.org/elpa/")
                    ("melpa" . "http://melpa.org/packages/")
                    ("melpa-stable" . "http://stable.melpa.org/packages/"))
 package-archive-priorities '(("melpa-stable" . 1)))

(package-initialize)
  (package-refresh-contents)
(when (not package-archive-contents)
  (package-install 'use-package))
(require 'use-package)

;; ui
(use-package dracula-theme
  :ensure t
  :config)

(use-package moe-theme)

(if (display-graphic-p)
    (load-theme 'dracula t)
    (load-theme 'moe-dark t))

(if (display-graphic-p)
    (scroll-bar-mode 0))

(tool-bar-mode 0)
(menu-bar-mode 0)

(use-package window-numbering
  :init (window-numbering-mode))

;; magit
(use-package magit)
(global-set-key (kbd "C-x g") 'magit-status)

;; evil
(use-package evil
  :init (evil-mode 1))

(use-package evil-escape
  :diminish evil-escape-mode
  :config (evil-escape-mode))

(define-key evil-normal-state-map (kbd "C-k") (lambda ()
            (interactive)
            (evil-scroll-up nil)))

(define-key evil-normal-state-map (kbd "C-j") (lambda ()
            (interactive)
            (evil-scroll-down nil)))
;; ensime
(add-to-list 'exec-path "/usr/local/bin")

(use-package ensime
  :ensure t
  :pin melpa-stable
  :bind ("M-RET" . ensime-edit-definition))

;; elm
(use-package elm-mode)

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
  :init (setq projectile-use-git-grep t)
  :config (projectile-global-mode t)
  :bind   (("M-f" . projectile-find-file)
           ("M-F" . projectile-find-file-in-known-projects)
           ("M-r" . helm-projectile-grep)))

(use-package helm-projectile
  :init
  (helm-projectile-on))

(setq projectile-switch-project-action 'helm-projectile-find-file)

;; weather
(use-package wttrin
  :ensure t
  :commands (wttrin)
  :init
  (setq wttrin-default-cities '("Sevilla" "Spain"))
  (setq wttrin-default-accept-language '("Accept-Language" . "en-EN")))

;; markdown
(use-package markdown-mode)

;; undo
(use-package undo-tree
  :diminish undo-tree-mode
  :config (global-undo-tree-mode)
  :bind ("M-/" . undo-tree-visualize))


;; zoom-window
(use-package zoom-window
  :bind ("C-x C-z" . zoom-window-zoom))

;; move-text
(use-package expand-region)
(use-package move-text)

(require 'expand-region)
(global-set-key (kbd "M-e") 'er/expand-region)
(global-set-key (kbd "M-E") 'er/contract-region)

;; neotree
 (use-package neotree
   :init (setq neo-smart-open t)
         (setq neo-autorefresh nil)
         (setq neo-theme 'arrow))

(defun neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (projectile-project-root))
        (file-name (buffer-file-name)))
    (neotree-toggle)
    (if project-dir
        (if (neo-global--window-exists-p)
            (progn
              (neotree-dir project-dir)
              (neotree-find file-name)))
              (message "Could not find git project root."))))

(global-set-key (kbd "C-c n") 'neotree-project-dir)

(evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
(evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)

;; which-key
(use-package which-key
  :init (which-key-mode))

(setq which-key-idle-delay 0.5)

;; windmove
(use-package windmove
  :init (windmove-default-keybindings))

;; rainbow-delimiters
(use-package rainbow-delimiters
  :init (rainbow-delimiters-mode))

;; git-gutter
(use-package git-gutter
  :init (global-git-gutter-mode +1))

;; snippets
(use-package yasnippet)
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-global-mode 1)

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
  (sp-pair "\"" "\"" :wrap "C-\"")
  (sp-pair "{" "}" :wrap "C-{"))

;; completions
(use-package company
  :diminish company-mode
  :commands company-mode
  :init
  (setq
   company-dabbrev-ignore-case nil
   company-dabbrev-code-ignore-case nil
   company-dabbrev-downcase nil
   company-idle-delay 0
   company-minimum-prefix-length 3)
  :config
  (define-key company-active-map [tab] nil)
  (define-key company-active-map (kbd "TAB") nil))

;; global keybindings
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "<home>") 'back-to-indentation)
(global-set-key (kbd "<end>") 'move-end-of-line)
(global-set-key (kbd "<S-up>") 'move-text-up)
(global-set-key (kbd "<S-down>") 'move-text-down)

(global-set-key (kbd "M-]") 'next-buffer)
(global-set-key (kbd "M-[") 'previous-buffer)

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
