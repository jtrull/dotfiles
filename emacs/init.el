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
(let ((available-fonts
       (or (font-family-list)
           (if (executable-find "fc-list")
               (delete-dups (process-lines "fc-list" ":mono" "-f" "%{family[0]}\\n"))))))
  (setq default-frame-alist
        `((width . 120)
	  (height . 65)
	  (menu-bar-lines . 0)
	  (tool-bar-lines . 0)
          (vertical-scroll-bars . nil)
          (horizontal-scroll-bars . nil)
	  (font . ,(cond
                    ((member "JetBrains Mono" available-fonts) "JetBrains Mono-14")
                    ((member "Consolas" available-fonts) "Consolas-14")
                    ((member "DejaVu Sans Mono" available-fonts) "DejaVu Sans Mono-14")))))
  (setq frame-title-format
        '((:eval (if (buffer-file-name)
                     (abbreviate-file-name (buffer-file-name))
                   "%b")))))

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
              indicate-empty-lines t
              show-trailing-whitespace t
              truncate-lines t)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; General keybinds
(global-set-key (kbd "C-j") (lambda () (interactive) (join-line t)))
(global-set-key (kbd "M-[") #'previous-buffer)
(global-set-key (kbd "M-]") #'next-buffer)
(windmove-default-keybindings)

;; Built-in global modes
(global-auto-revert-mode 1)
(global-hl-line-mode 1)
(pixel-scroll-mode 0)
(xterm-mouse-mode 1)

;; Packages
(setq use-package-always-pin "melpa-stable")

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

(use-package counsel
  :ensure t
  :pin gnu
  :demand
  :delight
  (ivy-mode)
  (counsel-mode)
  :bind ("C-s" . swiper)
  :config
  (ivy-mode 1)
  (counsel-mode 1))

(use-package ivy-prescient
  :ensure t
  :after counsel
  :config (ivy-prescient-mode 1))

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

(use-package ibuffer-projectile
  :ensure t
  :commands ibuffer-projectile-set-filter-groups)

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :config
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-projectile-set-filter-groups)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))

(use-package org
  :defer t
  :config
  (add-hook 'org-mode-hook #'org-indent-mode)
  (add-hook 'org-mode-hook #'visual-line-mode)
  (add-hook 'org-shiftup-final-hook #'windmove-up)
  (add-hook 'org-shiftdown-final-hook #'windmove-down)
  (add-hook 'org-shiftleft-final-hook #'windmove-left)
  (add-hook 'org-shiftright-final-hook #'windmove-right))

(use-package comint
  :defer t
  :config
  (add-hook 'comint-mode-hook
            (lambda ()
              (setq-local scroll-margin 0
                          show-trailing-whitespace nil))))

(use-package term
  :bind ("C-c t" . (lambda ()
                     (interactive)
                     (ansi-term (or explicit-shell-file-name
                                    (getenv "ESHELL")
                                    shell-file-name))))
  :config
  (unbind-key "M-x" term-raw-map)
  (add-hook 'term-mode-hook
            (lambda ()
              (setq-local global-hl-line-mode nil
                          scroll-margin 0
                          show-trailing-whitespace nil))))

(use-package projectile
  :ensure t
  :delight
  :config
  (projectile-mode 1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package counsel-projectile
  :ensure t
  :config (counsel-projectile-mode 1))

(use-package projectile-rails
  :ensure t
  :after projectile
  :config
  (projectile-rails-global-mode 1)
  (define-key projectile-rails-mode-map (kbd "C-c r") 'projectile-rails-command-map))

(use-package flycheck
  :ensure t
  :config (global-flycheck-mode 1))

(use-package company
  :ensure t
  :delight
  :config
  (global-company-mode 1)
  (bind-key "TAB" 'company-complete-selection company-active-map)
  (bind-key "ESC" 'company-abort company-active-map)
  (unbind-key "RET" company-active-map))

(use-package company-prescient
  :ensure t
  :after company
  :config (company-prescient-mode 1))

(use-package smartparens
  :ensure t
  :delight
  :config
  (require 'smartparens-config)
  (add-to-list 'sp-ignore-modes-list 'term-mode)
  (smartparens-global-mode 1))

(use-package magit
  :ensure t
  :defer t
  :config
  (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode 1)
  (diff-hl-flydiff-mode 1))

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

(use-package restclient
  :ensure t
  :pin melpa
  :defer t)

(use-package sql-indent
  :ensure t
  :pin gnu
  :delight sqlind-minor-mode
  :hook (sql . sqlind-minor-mode))

(use-package terraform-mode
  :ensure t
  :defer t
  :config
  (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode))

(use-package company-terraform
  :ensure t
  :after terraform-mode)

(use-package wgrep
  :ensure t
  :defer t)

(use-package yaml-mode
  :ensure t
  :bind (:map yaml-mode-map
              ("C-m" . newline-and-indent)))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;; init.el ends here
