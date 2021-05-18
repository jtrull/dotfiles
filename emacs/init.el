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

(defun jt/term ()
  "Start a new `ansi-term' with the default shell."
  (interactive)
  (require 'term)
  (ansi-term (or explicit-shell-file-name
                 (getenv "ESHELL")
                 shell-file-name)))

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
(straight-use-package 'blackout)
(require 'blackout)

;;
;; Eagerly loaded packages
;;

;; Dracula theme
(straight-use-package 'dracula-theme)
(load-theme 'dracula t)

;; Selectrum
(straight-use-package 'selectrum)
(straight-use-package 'selectrum-prescient)
(selectrum-mode 1)
(selectrum-prescient-mode 1)
(prescient-persist-mode 1)
;; Make current selection more obvious.
(set-face-attribute 'selectrum-current-candidate nil :inherit 'highlight)

;; Ctrlf
(straight-use-package 'ctrlf)
(ctrlf-mode 1)

;; diff-hl
(straight-use-package 'diff-hl)
(global-diff-hl-mode 1)
(diff-hl-flydiff-mode 1)

;; Marginalia
(straight-use-package 'marginalia)
(marginalia-mode 1)
(define-key minibuffer-local-map (kbd "M-A") #'marginalia-cycle)

;; Projectile
(straight-use-package 'projectile)
(straight-use-package 'projectile-rails)
(projectile-mode 1)
(projectile-rails-global-mode 1)
(blackout 'projectile-mode)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(define-key projectile-rails-mode-map (kbd "C-c r") 'projectile-rails-command-map)

;; Super-save
(straight-use-package 'super-save)
(super-save-mode 1)
(blackout 'super-save-mode)

;; Undo Tree
(straight-use-package 'undo-tree)
(global-undo-tree-mode 1)
(blackout 'undo-tree-mode)

;; Volatile highlights
(straight-use-package 'volatile-highlights)
(volatile-highlights-mode 1)
(blackout 'volatile-highlights-mode)

;; Which-key
(straight-use-package 'which-key)
(which-key-mode 1)
(blackout 'which-key-mode)

;; ws-butler
(straight-use-package 'ws-butler)
(ws-butler-mode 1)
(blackout 'ws-butler-mode)

;;
;; Demand-loaded packages
;;

;; ibuffer
(straight-use-package 'ibuffer-projectile)
(global-set-key (kbd "C-x C-b") #'ibuffer)
(with-eval-after-load 'ibuffer
  (require 'ibuffer-projectile)
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-projectile-set-filter-groups)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))

;; Org
(with-eval-after-load 'org
  (add-hook 'org-mode-hook #'org-indent-mode)
  (add-hook 'org-mode-hook #'visual-line-mode)
  (add-hook 'org-shiftup-final-hook #'windmove-up)
  (add-hook 'org-shiftdown-final-hook #'windmove-down)
  (add-hook 'org-shiftleft-final-hook #'windmove-left)
  (add-hook 'org-shiftright-final-hook #'windmove-right))

;; ANSI color
(with-eval-after-load 'ansi-color
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

;; Comint
(with-eval-after-load 'comint
  (add-hook 'comint-mode-hook #'jt/disable-scroll-margin))

;; Compile
(with-eval-after-load 'compile
  (require 'ansi-color)
  ;; From https://endlessparentheses.com/ansi-colors-in-the-compilation-buffer-output.html
  (defun jt/colorize-compilation ()
    "Colorize from `compilation-filter-start' to `point'."
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region compilation-filter-start (point))))
  (add-hook 'compilation-filter-hook #'jt/colorize-compilation)
  (add-hook 'compilation-mode-hook #'jt/disable-scroll-margin)
  (setq compilation-scroll-output 'first-error))

;; term
(global-set-key (kbd "C-c t") #'jt/term)
(with-eval-after-load 'term
  (define-key term-raw-map (kbd "M-x") nil)
  (define-key term-raw-map (kbd "M-[") nil)
  (define-key term-raw-map (kbd "M-]") nil)
  (add-hook 'term-mode-hook
            (lambda ()
              (setq-local global-hl-line-mode nil)
              (jt/disable-scroll-margin))))

(straight-use-package 'flycheck)
(add-hook 'prog-mode-hook #'flycheck-mode)

(straight-use-package 'company)
(straight-use-package 'company-prescient)
(add-hook 'prog-mode-hook #'company-mode)
(add-hook 'company-mode-hook #'company-prescient-mode)
(with-eval-after-load 'company
  (blackout 'company-mode)
  (define-key company-active-map (kbd "TAB") #'company-complete-selection)
  (define-key company-active-map (kbd "ESC") #'company-abort)
  (define-key company-active-map (kbd "RET") nil))

(straight-use-package 'expand-region)
(global-set-key (kbd "C-=") #'er/expand-region)

(straight-use-package 'smartparens)
(add-hook 'prog-mode-hook #'smartparens-mode)
(with-eval-after-load 'smartparens
  (require 'smartparens-config)
  (blackout 'smartparens-mode))

(straight-use-package 'magit)
(global-set-key (kbd "C-x g") #'magit-status)
(with-eval-after-load 'magit
  (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

(straight-use-package 'treemacs)
(straight-use-package 'treemacs-magit)
(straight-use-package 'treemacs-projectile)
(with-eval-after-load 'treemacs
  (require 'treemacs-magit)
  (require 'treemacs-projectile))

(straight-use-package 'lsp-mode)
(straight-use-package 'lsp-ui)
(straight-use-package 'lsp-treemacs)
(straight-use-package 'lsp-java)
(setq lsp-keymap-prefix "C-c l"
      lsp-enable-snippet nil)
(with-eval-after-load 'lsp-mode
  (require 'lsp-ui)
  (require 'dap-mode)
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (add-hook 'lsp-mode-hook #'lsp-ui-mode)
  (dap-auto-configure-mode)
  (with-eval-after-load 'treemacs
    (require 'lsp-treemacs)))
(with-eval-after-load 'lsp-java
  (setq lsp-java-java-path "/home/jtrull/.asdf/installs/java/adoptopenjdk-11.0.9+11/bin/java"))
(add-hook 'java-mode-hook (lambda () (require 'lsp-java) (lsp)))

(straight-use-package 'deadgrep)
(straight-use-package 'docker)
(straight-use-package 'dockerfile-mode)
(straight-use-package 'docker-compose-mode)

(straight-use-package 'kubernetes)
(add-hook 'kubernetes-logs-mode #'jt/disable-scroll-margin)

(straight-use-package 'restclient)
(straight-use-package 'ripgrep)

(straight-use-package 'sql-indent)
(with-eval-after-load 'sql-indent
  (blackout 'sqlind-minor-mode))
(add-hook 'sql-mode-hook #'sqlind-minor-mode)

(straight-use-package 'terraform-mode)
(straight-use-package 'company-terraform)
(with-eval-after-load 'terraform-mode
  (require 'company-terraform)
  (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode))

(straight-use-package 'yaml-mode)
(with-eval-after-load 'yaml-mode
  (define-key yaml-mode-map (kbd "C-m") #'newline-and-indent))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;; init.el ends here
