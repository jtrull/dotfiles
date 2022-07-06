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

;; Local files
(require 'xdg)
(defconst jt/emacs-local-directory (expand-file-name "emacs" (xdg-data-home))
  "XDG directory for Emacs local data.")
(unless (file-exists-p jt/emacs-local-directory)
  (make-directory jt/emacs-local-directory))

;; Window configuration
(setq default-frame-alist
      `((width . 120)
	(height . 65)
	(menu-bar-lines . 0)
	(tool-bar-lines . 0)
        (vertical-scroll-bars . nil)
        (horizontal-scroll-bars . nil))
      frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(defvar jt/acceptable-font-families
  '("JetBrains Mono" "Consolas" "DejaVu Sans Mono")
  "List of acceptable font families in priority order.")

(defvar jt/default-font-family
  (let ((available-fonts
         (or (font-family-list)
             (if (executable-find "fc-list")
                 (delete-dups (process-lines "fc-list" ":mono" "-f" "%{family[0]}\\n"))))))
    (seq-find (lambda (font) (member font available-fonts)) jt/acceptable-font-families))
  "Font family used for new frames.")

(defvar jt/default-font-height-mms 3.0 "Default font height in millimeters.")

(defun jt/update-frame-font (&optional frame keep-default)
  "Set FRAME's font based on display resolution.

If FRAME is not specified, update the currently selected frame.  If optional
argument KEEP-DEFAULT is non-nil, then also update `default-frame-alist'."
  (interactive)
  (let* ((selected-frame (or frame (selected-frame)))
         (frame-mm-height (nth 1 (frame-monitor-attribute 'mm-size selected-frame)))
         (frame-pixel-height (nth 3 (frame-monitor-attribute 'geometry selected-frame)))
         (pixels-per-mm (/ (float frame-pixel-height) (float frame-mm-height)))
         (font-height (round (* jt/default-font-height-mms pixels-per-mm)))
         (font (format "%s-%d" jt/default-font-family font-height)))
    (set-frame-font font nil (list selected-frame) t)
    (unless keep-default
      (setf (alist-get 'font default-frame-alist) font))))

(add-hook 'after-make-frame-functions #'jt/update-frame-font)

;; General settings
(setq inhibit-startup-screen t
      make-backup-files nil
      ring-bell-function 'ignore
      disabled-command-function nil
      native-comp-async-report-warnings-errors 'silent
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
(global-set-key (kbd "C-x 2") (lambda () (interactive) (select-window (split-window-below))))
(global-set-key (kbd "C-x 3") (lambda () (interactive) (select-window (split-window-right))))
;; FIXME: Binding "M-[" is problematic when running Emacs in a terminal.
;; It conflicts with key translactions for PgUp, PgDn, Insert, Delete, and more
(global-set-key (kbd "M-[") #'previous-buffer)
(global-set-key (kbd "M-]") #'next-buffer)

;; Built-in global modes
(global-auto-revert-mode 1)
(global-hl-line-mode 1)
(show-paren-mode 1)
(xterm-mouse-mode 1)

;; Auto-saves
(let ((jt/auto-save-directory (expand-file-name "autosaves" jt/emacs-local-directory)))
  (unless (file-exists-p jt/auto-save-directory)
    (make-directory jt/auto-save-directory))
  (setq auto-save-file-name-transforms
        `((".*" ,jt/auto-save-directory t))))

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

;; Hydra
(straight-use-package 'hydra)

;; Completion framework
(straight-use-package 'vertico)
(straight-use-package 'orderless)
(straight-use-package 'marginalia)
(vertico-mode 1)
(marginalia-mode 1)
(require 'orderless)
(setq completion-styles '(orderless basic)
      completion-category-overrides '((file (styles basic partial-completion))))
(define-key minibuffer-local-map (kbd "M-A") #'marginalia-cycle)
(require 'savehist)
(setq savehist-file (expand-file-name "history" jt/emacs-local-directory))
(savehist-mode 1)

;; Actions
(straight-use-package 'embark)
(straight-use-package 'consult)
(straight-use-package 'embark-consult)
(global-set-key (kbd "C-.") #'embark-dwim)
(global-set-key (kbd "M-.") #'embark-act)

;; Ctrlf
(straight-use-package 'ctrlf)
(ctrlf-mode 1)

;; diff-hl
(straight-use-package 'diff-hl)
(global-diff-hl-mode 1)
(diff-hl-flydiff-mode 1)

;; Super-save
(straight-use-package 'super-save)
(require 'super-save)
(add-to-list 'super-save-hook-triggers 'find-file-hook)
(add-to-list 'super-save-triggers 'ace-window)
(setq super-save-remote-files nil)
(super-save-mode 1)
(blackout 'super-save-mode)

;; Tree sitter
(straight-use-package 'tree-sitter)
(straight-use-package 'tree-sitter-langs)
(global-tree-sitter-mode 1)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
(blackout 'tree-sitter-mode " 🌲")

;; Undo Tree
(straight-use-package 'undo-tree)
(global-undo-tree-mode 1)
(let ((jt/undo-save-directory (expand-file-name "emacs/undo-tree" (xdg-data-home))))
  (unless (file-exists-p jt/undo-save-directory)
    (make-directory jt/undo-save-directory))
  (setq undo-tree-auto-save-history t
        undo-tree-history-directory-alist `(("." . ,jt/undo-save-directory))))
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
(ws-butler-global-mode 1)
(setq ws-butler-global-exempt-modes
      (append ws-butler-global-exempt-modes
              '(term-mode comint-mode)))
(blackout 'ws-butler-mode)

;;
;; Demand-loaded packages
;;

;; Ace Window
(straight-use-package 'ace-window)
(global-set-key (kbd "M-o") #'ace-window)
(with-eval-after-load 'ace-window
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-ignore-current nil
        aw-scope 'frame)
  (set-face-attribute 'aw-leading-char-face nil
                      :height 4.0 :weight 'bold))

;; Markdown
(with-eval-after-load 'markdown-mode
  (add-hook 'markdown-mode-hook #'visual-line-mode))

;; Org
(with-eval-after-load 'org
  (add-hook 'org-mode-hook #'org-indent-mode)
  (add-hook 'org-mode-hook #'visual-line-mode))

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

;; crux
(straight-use-package 'crux)
(global-set-key [remap move-beginning-of-line] #'crux-move-beginning-of-line)
(global-set-key (kbd "C-j") #'crux-top-join-line)
(global-set-key (kbd "C-k") #'crux-kill-and-join-forward)

;; Ediff
(with-eval-after-load 'ediff
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally))

;; ejc-sql
(when (executable-find "lein")
  (straight-use-package 'ejc-sql)
  (add-hook 'sql-mode-hook #'ejc-sql-mode)
  (with-eval-after-load 'ejc-sql
    (setq ejc-completion-system 'standard)
    (setq ejc-jdbc-drivers
          '("oracle" [com\.oracle\.database\.jdbc/ojdbc11 "21.5.0.0"]))
    (add-hook 'ejc-sql-minor-mode-hook
              (lambda ()
                (ejc-eldoc-setup)))
    (add-hook 'ejc-sql-connected-hook
              (lambda ()
                (ejc-set-fetch-size 100)
                (ejc-set-max-rows 1000)
                (ejc-set-show-too-many-rows-message t)
                (ejc-set-column-width-limit 40)
                (ejc-set-use-unicode t)))))

;; Eshell
(with-eval-after-load 'eshell
  (add-hook 'eshell-mode-hook #'jt/disable-scroll-margin))

;; term
(with-eval-after-load 'term
  (define-key term-raw-map (kbd "M-o") nil)
  (define-key term-raw-map (kbd "M-w") nil)
  (define-key term-raw-map (kbd "C-y") #'term-paste)
  (define-key term-raw-map (kbd "M-x") nil)
  (define-key term-raw-map (kbd "M-[") nil)
  (define-key term-raw-map (kbd "M-]") nil)
  (add-hook 'term-mode-hook
            (lambda ()
              (setq-local global-hl-line-mode nil)
              (jt/disable-scroll-margin))))

;; Flycheck
(straight-use-package 'flycheck)
(add-hook 'prog-mode-hook #'flycheck-mode)

;; CFML
(straight-use-package 'cfml-mode)
(add-to-list 'auto-mode-alist '("\\.cfml\\'" . cfml-mode))

;; Company
(straight-use-package 'company)
(add-hook 'prog-mode-hook #'company-mode)
(with-eval-after-load 'company
  (blackout 'company-mode)
  (define-key company-active-map [tab] #'company-complete-selection)
  (define-key company-active-map (kbd "TAB") #'company-complete-selection)
  (define-key company-active-map (kbd "ESC") #'company-abort)
  (define-key company-active-map [return] nil)
  (define-key company-active-map (kbd "RET") nil))

;; Darkroom
(straight-use-package 'darkroom)

;; eww
(with-eval-after-load 'eww
  (add-hook 'eww-mode-hook
            (lambda ()
              (setq show-trailing-whitespace nil))))

;; Expand-region
(straight-use-package 'expand-region)
(global-set-key (kbd "C-=") #'er/expand-region)

;; Focus
(straight-use-package 'focus)
(with-eval-after-load 'focus
  (setq focus-mode-to-thing
        '((prog-mode . defun)
          (text-mode . paragraph))))

;; Smartparens
(straight-use-package 'smartparens)
(add-hook 'prog-mode-hook #'smartparens-mode)
(with-eval-after-load 'smartparens
  (require 'smartparens-config)
  (blackout 'smartparens-mode))

;; Magit
(straight-use-package 'magit)
(global-set-key (kbd "C-x g b") #'magit-blame)
(global-set-key (kbd "C-x g g") #'magit-status)
(global-set-key (kbd "C-x g l") #'magit-log-buffer-file)
(add-to-list 'display-buffer-alist
             '("^magit: "
               (display-buffer-reuse-window display-buffer-same-window)))
(with-eval-after-load 'magit
  (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
  (setq git-commit-summary-max-length 50))

;; Treemacs
(straight-use-package 'treemacs)
(straight-use-package 'treemacs-magit)
(global-set-key (kbd "M-0") #'treemacs-select-window)
(with-eval-after-load 'treemacs
  (require 'treemacs-magit))

;; Origami
(straight-use-package 'origami)
(with-eval-after-load 'origami
  (define-key origami-mode-map (kbd "C-c f f") #'origami-toggle-node)
  (define-key origami-mode-map (kbd "C-c f C-f") #'origami-toggle-all-nodes)
  (define-key origami-mode-map (kbd "C-c C-f C-f") #'origami-toggle-all-nodes)
  (define-key origami-mode-map (kbd "C-c f F") #'origami-recursively-toggle-node)
  (define-key origami-mode-map (kbd "C-c f o") #'origami-open-node)
  (define-key origami-mode-map (kbd "C-c f C-o") #'origami-open-all-nodes)
  (define-key origami-mode-map (kbd "C-c f O") #'origami-open-node-recursively)
  (define-key origami-mode-map (kbd "C-c f c") #'origami-close-node)
  (define-key origami-mode-map (kbd "C-c f C-c") #'origami-close-all-nodes)
  (define-key origami-mode-map (kbd "C-c f C") #'origami-close-node-recursively))

;; LSP
(straight-use-package 'lsp-mode)
(straight-use-package 'lsp-ui)
(straight-use-package 'lsp-treemacs)
(straight-use-package 'lsp-origami)
(straight-use-package 'lsp-java)
(setq lsp-keymap-prefix "C-c l"
      lsp-enable-snippet nil)
(add-hook 'ruby-mode-hook #'lsp)
(with-eval-after-load 'lsp-mode
  (require 'lsp-ui)
  (require 'lsp-treemacs)
  (require 'dap-mode)
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (add-hook 'lsp-mode-hook #'lsp-ui-mode)
  (add-hook 'lsp-after-open-hook #'lsp-origami-try-enable)
  (dap-auto-configure-mode)
  (setq lsp-solargraph-use-bundler nil))
(with-eval-after-load 'cc-mode
  (require 'lsp-java)
  (setq lsp-java-java-path "/home/jtrull/.asdf/installs/java/adoptopenjdk-11.0.9+11/bin/java")
  (add-hook 'java-mode-hook #'lsp))

;; Ripgrep front-ends
(straight-use-package 'deadgrep)
(straight-use-package 'ripgrep)

;; Docker
(straight-use-package 'docker)
(straight-use-package 'dockerfile-mode)

;; Kubernetes
(straight-use-package 'kubernetes)
(add-hook 'kubernetes-logs-mode #'jt/disable-scroll-margin)

(straight-use-package 'kubel) ;; alternative to kubernetes-el

;; Restclient
(straight-use-package 'restclient)

;; Ruby
(with-eval-after-load 'ruby-mode
  (add-hook 'ruby-mode-hook #'abbrev-mode)
  (setq ruby-align-chained-calls t))

;; SGML
(setq sgml-quick-keys t) ;; must be set before loading for full effect
(with-eval-after-load 'sgml-mode
  (add-hook 'sgml-mode-hook #'display-line-numbers-mode))

;; SQL
(with-eval-after-load 'sql
  (if (executable-find "sqlcl")
      (setq sql-oracle-program "sqlcl")))

;; SQL indentation
(straight-use-package 'sql-indent)
(with-eval-after-load 'sql-indent
  (blackout 'sqlind-minor-mode))
(add-hook 'sql-mode-hook #'sqlind-minor-mode)

;; Terraform
(straight-use-package 'terraform-mode)
(straight-use-package 'company-terraform)
(with-eval-after-load 'terraform-mode
  (require 'company-terraform)
  (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode))

;; vterm
(straight-use-package 'vterm)
(global-set-key (kbd "C-c t") #'vterm)
(with-eval-after-load 'vterm
  (setq vterm-max-scrollback 8192)
  (add-hook 'vterm-mode-hook
            (lambda ()
              (setq-local global-hl-line-mode nil)
              (jt/disable-scroll-margin))))

;; YAML mode
(straight-use-package 'yaml-mode)
(with-eval-after-load 'yaml-mode
  (add-hook 'yaml-mode-hook #'display-line-numbers-mode)
  (add-hook 'yaml-mode-hook #'lsp)
  (define-key yaml-mode-map (kbd "C-m") #'newline-and-indent))

;; YARD mode
(straight-use-package 'yard-mode)
(add-hook 'ruby-mode-hook #'yard-mode)
(with-eval-after-load 'yard-mode
  (blackout 'yard-mode))

;; Get that custom crap out of here :-)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;; init.el ends here

;; Local Variables:
;; coding: utf-8
;; End:
