;;; init.el --- Jonathan's Emacs configuration

;;; Commentary:

;; This is my personal Emacs configuration.  You probably don't want any.

;;; Code:

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

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

;; Common overrides
(defun jt/disable-scroll-margin ()
  "Remove scroll margin and trailing whitespace highlighting in the current buffer."
  (setq-local scroll-margin 0
              show-trailing-whitespace nil))

;; General keybinds
(global-set-key (kbd "C-j") (lambda () (interactive) (join-line t)))
(global-set-key (kbd "C-x 2") (lambda () (interactive) (select-window (split-window-below))))
(global-set-key (kbd "C-x 3") (lambda () (interactive) (select-window (split-window-right))))
;; FIXME: Binding "M-[" is problematic when running Emacs in a terminal.
;; It conflicts with key translactions for PgUp, PgDn, Insert, Delete, and more
(global-set-key (kbd "M-[") #'previous-buffer)
(global-set-key (kbd "M-]") #'next-buffer)
(windmove-default-keybindings)

;; Built-in global modes
(global-auto-revert-mode 1)
(global-hl-line-mode 1)
(show-paren-mode 1)
(xterm-mouse-mode 1)

;; prog-mode hooks
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Packages
(straight-use-package 'use-package)
(straight-use-package 'blackout)
(eval-when-compile (require 'use-package))
(require 'blackout)
(require 'bind-key)

(use-package dracula-theme
  :straight t
  :config (load-theme 'dracula t))

(use-package selectrum
  :straight t
  :config
  (selectrum-mode 1)
  ;; Make current selection more obvious.
  (set-face-attribute 'selectrum-current-candidate nil :inherit 'highlight)
  (use-package selectrum-prescient
    :straight t
    :config
    (selectrum-prescient-mode 1)
    (prescient-persist-mode 1)))

(use-package ctrlf
  :straight t
  :config (ctrlf-mode 1))

(use-package diff-hl
  :straight t
  :config
  (global-diff-hl-mode 1)
  (diff-hl-flydiff-mode 1))

(use-package expand-region
  :straight t
  :bind ("C-=" . #'er/expand-region))

(use-package marginalia
  :straight t
  :config
  (marginalia-mode 1)
  (bind-key "M-A" #'marginalia-cycle minibuffer-local-map))

(use-package super-save
  :straight t
  :blackout t
  :config (super-save-mode 1))

(use-package undo-tree
  :straight t
  :blackout t
  :config (global-undo-tree-mode 1))

(use-package volatile-highlights
  :straight t
  :blackout t
  :config (volatile-highlights-mode 1))

(use-package which-key
  :straight t
  :blackout t
  :config (which-key-mode 1))

(use-package ws-butler
  :straight t
  :blackout t
  :config (ws-butler-mode 1))

(use-package ibuffer
  :bind ("C-x C-b" . #'ibuffer)
  :config
  (use-package ibuffer-projectile :straight t)
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

(use-package ansi-color
  :defer t
  :config
  ;; Set ansi colors from current theme.
  (require 'term)
  (setq ansi-color-names-vector
        (vector (face-attribute 'term-color-black :foreground)
                (face-attribute 'term-color-red :foreground)
                (face-attribute 'term-color-green :foreground)
                (face-attribute 'term-color-yellow :foreground)
                (face-attribute 'term-color-blue :foreground)
                (face-attribute 'term-color-magenta :foreground)
                (face-attribute 'term-color-cyan :foreground)
                (face-attribute 'term-color-white :foreground)))
  (setq ansi-color-map (ansi-color-make-color-map)))

(use-package comint
  :defer t
  :config
  (add-hook 'comint-mode-hook #'jt/disable-scroll-margin))

(use-package compile
  :defer t
  :config
  (require 'ansi-color)
  ;; From https://endlessparentheses.com/ansi-colors-in-the-compilation-buffer-output.html
  (defun jt/colorize-compilation ()
    "Colorize from `compilation-filter-start' to `point'."
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region compilation-filter-start (point))))
  (add-hook 'compilation-filter-hook #'jt/colorize-compilation)
  (add-hook 'compilation-mode-hook #'jt/disable-scroll-margin)
  (setq compilation-scroll-output 'first-error))

(use-package term
  :bind ("C-c t" . (lambda ()
                     (interactive)
                     (ansi-term (or explicit-shell-file-name
                                    (getenv "ESHELL")
                                    shell-file-name))))
  :config
  (unbind-key "M-x" term-raw-map)
  (unbind-key "M-[" term-raw-map)
  (unbind-key "M-]" term-raw-map)
  (add-hook 'term-mode-hook
            (lambda ()
              (setq-local global-hl-line-mode nil)
              (jt/disable-scroll-margin))))

(use-package projectile
  :straight t
  :blackout t
  :demand t
  :bind-keymap ("C-c p" . projectile-command-map)
  :config (projectile-mode 1))

(use-package projectile-rails
  :straight t
  :demand t
  :bind-keymap ("C-c r" . projectile-rails-command-map)
  :config (projectile-rails-global-mode 1))

(use-package flycheck
  :straight t
  :config (global-flycheck-mode 1))

(use-package company
  :straight t
  :blackout t
  :demand t
  :bind (:map company-active-map
              ("TAB" . company-complete-selection)
              ("ESC" . company-abort))
  :config
  (unbind-key "RET" company-active-map)
  (global-company-mode 1)
  (use-package company-prescient
    :straight t
    :config (company-prescient-mode 1)))

(use-package smartparens
  :straight t
  :blackout t
  :config
  (require 'smartparens-config)
  (add-to-list 'sp-ignore-modes-list 'term-mode)
  (smartparens-global-mode 1))

(use-package magit
  :straight t
  :bind ("C-x g" . magit-status)
  :config
  (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

(use-package treemacs
  :straight t
  :commands treemacs
  :config
  (use-package treemacs-magit :straight t :demand t)
  (use-package treemacs-projectile :straight t :demand t))

(use-package lsp-mode
  :straight t
  :commands lsp
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-enable-snippet nil)
  (use-package dap-mode :straight t)
  (use-package lsp-ui :straight t)
  (use-package lsp-treemacs :straight t :after treemacs)
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (add-hook 'lsp-mode-hook #'lsp-ui-mode)
  (dap-auto-configure-mode))

(use-package lsp-java
  :straight t
  :defer t
  :init
  (add-hook 'java-mode-hook
            (lambda ()
              (require 'lsp-java)
              (lsp)))
  :config
  (setq lsp-java-java-path "/home/jtrull/.asdf/installs/java/adoptopenjdk-11.0.9+11/bin/java"))

(use-package deadgrep :straight t :defer t)
(use-package docker :straight t :defer t)
(use-package dockerfile-mode :straight t :defer t)
(use-package docker-compose-mode :straight t :defer t)

(use-package kubernetes
  :straight t
  :defer t
  :hook (kubernetes-logs-mode . jt/disable-scroll-margin))

(use-package restclient :straight t :commands restclient)
(use-package ripgrep :straight t :defer t)

(use-package sql-indent
  :straight t
  :blackout t
  :hook (sql-mode . sqlind-minor-mode))

(use-package terraform-mode
  :straight t
  :defer t
  :hook (terraform-mode . terraform-format-on-save-mode)
  :config
  (use-package company-terraform :straight t))

(use-package yaml-mode
  :straight t
  :defer t
  :bind (:map yaml-mode-map
              ("C-m" . newline-and-indent)))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;; init.el ends here
