;;; init.el --- Jonathan's Emacs configuration

;;; Commentary:

;; This is my personal Emacs configuration.  You probably don't want any.

;;; Code:

(when (eval-when-compile (version< emacs-version "27"))
  (load "~/.config/emacs/early-init.el")
  (package-initialize))

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Window configuration
(setq default-frame-alist
      `((width . 120)
	(height . 65)
	(menu-bar-lines . 0)
	(tool-bar-lines . 0)
        (vertical-scroll-bars . nil)
        (horizontal-scroll-bars . nil)
	(font . ,(cond
                  ((member "JetBrains Mono" (font-family-list)) "JetBrains Mono-14")
                  ((member "Consolas" (font-family-list)) "Consolas-14")
                  ((member "DejaVu Sans Mono" (font-family-list)) "DejaVu Sans Mono-14")))))
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; General settings
(setq inhibit-startup-screen t
      make-backup-files nil
      ring-bell-function 'ignore
      uniquify-buffer-name-style 'forward
      uniquify-separator "/"
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")

;; Modeline
(line-number-mode 1)
(column-number-mode 1)
(size-indication-mode 1)

;; Scrolling
(setq mouse-wheel-follow-mouse t
      mouse-wheel-scroll-amount '((nil . 1))
      scroll-bar-adjust-thumb-portion nil
      scroll-conservatively 1000
      scroll-error-top-bottom t
      scroll-margin 3
      scroll-preserve-screen-position t)

;; General buffer defaults
(setq-default indent-tabs-mode nil
              truncate-lines t)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; General keybinds
(global-set-key (kbd "C-j") (lambda () (interactive) (join-line t)))
(global-set-key (kbd "C-x C-b") #'ibuffer)
(windmove-default-keybindings)

;; Built-in global modes
(global-auto-revert-mode 1)
(global-hl-line-mode 1)
(xterm-mouse-mode 1)

;; Packages
(use-package delight
  :ensure t
  :pin gnu)

(use-package dracula-theme
  :ensure t
  :config (load-theme 'dracula t))

(use-package super-save
  :ensure t
  :delight
  :config
  (super-save-mode 1))

(use-package hydra
  :ensure t
  :pin gnu)

(use-package flx
  :ensure t)

(use-package counsel
  :ensure t
  :pin gnu
  :demand
  :delight
  (ivy-mode)
  (counsel-mode)
  :bind ("C-s" . swiper)
  :config
  (setq ivy-re-builders-alist
        '((t . ivy--regex-fuzzy)))
  (ivy-mode 1)
  (counsel-mode 1))

(use-package which-key
  :ensure t
  :pin gnu
  :delight
  :config (which-key-mode 1))

(use-package expand-region
  :ensure t
  :pin gnu
  :bind ("C-=" . er/expand-region))

(use-package undo-tree
  :ensure t
  :pin gnu
  :delight
  :config (global-undo-tree-mode 1))

(use-package org
  :mode (("\\.org\\'" . org-mode))
  :config
  (add-hook 'org-mode-hook #'org-indent-mode)
  (add-hook 'org-mode-hook #'visual-line-mode)
  (add-hook 'org-shiftup-final-hook #'windmove-up)
  (add-hook 'org-shiftdown-final-hook #'windmove-down)
  (add-hook 'org-shiftleft-final-hook #'windmove-left)
  (add-hook 'org-shiftright-final-hook #'windmove-right))

(use-package term
  :defer t
  :config
  (add-hook 'term-mode-hook (lambda () (visual-line-mode nil))))

(use-package projectile
  :ensure t
  :config
  (projectile-mode 1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package counsel-projectile
  :ensure t
  :config (counsel-projectile-mode 1))

(use-package flycheck
  :ensure t
  :config (global-flycheck-mode 1))

(use-package company
  :ensure t
  :delight
  :config (global-company-mode 1))

(use-package smartparens
  :ensure t
  :delight
  :config
  (require 'smartparens-config)
  (add-to-list 'sp-ignore-modes-list 'term-mode)
  (smartparens-global-mode 1))

(use-package magit
  :ensure t)

(use-package treemacs
  :ensure t
  :commands treemacs)

(use-package treemacs-magit
  :ensure t
  :after treemacs magit)

(use-package treemacs-projectile
  :ensure t
  :after treemacs projectile)

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-enable-snippet nil)
  :hook (lsp-mode . lsp-enable-which-key-integration)
  :commands lsp)

(use-package dap-mode
  :ensure t
  :after lsp-mode
  :config (dap-auto-configure-mode))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list)

(use-package lsp-java
  :ensure t
  :hook (java-mode . lsp)
  :config
  (setq lsp-java-java-path "/home/jtrull/.asdf/installs/java/adoptopenjdk-11.0.9+11/bin/java"))

(use-package terraform-mode
  :ensure t
  :defer t)

(use-package company-terraform
  :ensure t
  :after terraform-mode)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;; init.el ends here
