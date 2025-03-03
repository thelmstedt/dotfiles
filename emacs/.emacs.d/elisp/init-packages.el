;; core utilities

(use-package diminish)                           ;; hide minor modes from modeline

(use-package editorconfig)                       ;; support for .editorconfig files
(use-package find-file-in-project)               ;; quickly find files in projects

(use-package desktop                             ;; restore state on reopen
  :init
  (desktop-save-mode 1)
  :custom
  (desktop-restore-eager 5)                      ;; restore first 5 buffers eagerly
  (desktop-auto-save-timeout 30)                 ;; save desktop every 30 seconds
  (desktop-load-locked-desktop t)                ;; load even if locked
  (desktop-save t)                               ;; always save
  :config
  (setq desktop-path (list user-emacs-directory)
        desktop-dirname user-emacs-directory
        desktop-base-file-name "emacs.desktop"))


;; completion system
(use-package vertico                             ;; vertical completion UI for minibuffer
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t))

(use-package marginalia                          ;; add helpful annotations to completion candidates
  :init
  (marginalia-mode))

(use-package consult
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))
  :hook (completion-list-mode . consult-preview-at-point-mode))

(use-package orderless                           ;; powerful completion style for fuzzy matching
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  :init
  (setq completion-category-defaults nil))

(use-package embark                              ;; context menu for completions
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult                      ;; integration between embark and consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; navigation and search
(use-package ace-window)                         ;; efficient window switching
(use-package avy)                                ;; jump to visible text using char-based search

(use-package imenu-anywhere)                     ;; search for symbols across buffers

;;treemacs

(use-package all-the-icons
  :if (display-graphic-p)
  :config
  (when (not (member "all-the-icons" (font-family-list)))
    (all-the-icons-install-fonts t)))

(use-package treemacs
  :bind
  ("C-1"       . treemacs-select-window)
  ("M-0"       . treemacs)
  ("C-x t 1"   . treemacs-delete-other-windows)
  ("C-x t t"   . treemacs)
  ("C-x t d"   . treemacs-select-directory)
  ("C-x t B"   . treemacs-bookmark)
  ("C-x t C-t" . treemacs-find-file)
  ("C-x t M-t" . treemacs-find-tag)
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-git-mode 'deferred)
  (treemacs-fringe-indicator-mode 'always)
  :custom
  (treemacs-sorting 'alphabetic-asc)
  (treemacs-is-never-other-window t)
  (treemacs-width 35))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))

(use-package treemacs-all-the-icons
  :after (treemacs all-the-icons)
  :config
  (treemacs-load-theme "all-the-icons"))

(use-package treemacs-tab-bar
  :after (treemacs)
  :config (treemacs-set-scope-type 'Tabs))



(use-package which-key                           ;; display available keybindings
  :config (which-key-mode))

;; search
(use-package rg                                  ;; ripgrep interface
  :config
  (rg-enable-default-bindings))

;; editing



(use-package smartparens                         ;; automatic insertion of paired delimiters
  :hook ((clojure-mode
          emacs-lisp-mode
          eval-expression-minibuffer-setup
          ielm-mode
          lisp-mode
          lisp-interaction-mode
          scheme-mode) . smartparens-strict-mode)
  :config
  (require 'smartparens-config)
  (sp-use-paredit-bindings)
  (show-smartparens-global-mode t))

(use-package undo-tree                           ;; better undo/redo visualization
  :config
  (global-undo-tree-mode)
  (defalias 'redo 'undo-tree-redo)
  :bind
  ("s-z" . undo)
  ("s-S-z" . redo))


;; syntax highlighting
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)  ; ask before installing grammars
  :config
  (global-treesit-auto-mode)

  ;; map legacy modes to tree-sitter ones
  (setq major-mode-remap-alist
        '((python-mode . python-ts-mode)
          (javascript-mode . js-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (json-mode . json-ts-mode)
          (css-mode . css-ts-mode)
          (yaml-mode . yaml-ts-mode)
          (bash-mode . bash-ts-mode)
          (rust-mode . rust-ts-mode)))

  ;; languages to auto-install
  (setq treesit-auto-langs
        '(bash
          cpp
          css
          elisp
          go
          html
          javascript
          json
          python
          rust
          typescript
          yaml))

  ;; optional but recommended font locks
  (setq treesit-font-lock-level 4))


;; git and version control
(use-package diff-hl)                            ;; highlight uncommitted changes
(use-package magit                               ;; complete git interface
  :bind ("C-x C-z" . magit-status))
(use-package forge                               ;; github integration for magit
  :after magit)

;; project management
(use-package projectile                          ;; project interaction library
  :config
  (setq projectile-use-git-grep t))

;; modes for specific file types
(use-package deft                                ;; quick note taking and searching
  :custom
  (deft-directory "~/Dropbox/deft")
  (deft-extension "org")
  (deft-text-mode 'org-mode)
  :bind ([f1] . deft)
  :hook (deft-mode . deft-filter-clear)
  :config
  (define-key deft-mode-map (kbd "C-w") 'deft-filter-clear))

(use-package markdown-mode                       ;; markdown editing
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode)))

;; built-in customizations
(use-package paren                               ;; highlight matching parentheses
  :config
  (show-paren-mode t)
  (setq show-paren-style 'parenthesis)
  (setq show-paren-delay 0))

;; shell stuff
(add-to-list 'auto-mode-alist '("\\.zsh$" . sh-mode))

;; syntax checking
(use-package flycheck)                           ;; on-the-fly syntax checking
(use-package hl-todo)                            ;; highlight TODO/FIXME/etc comments

;; themes and visual
(use-package zenburn-theme                       ;; low contrast color theme
  :config
  (load-theme 'zenburn t))


;; terminal
(use-package vterm                               ;; better terminal
  :bind ("C-c t" . vterm))

;; server
(use-package server                              ;; emacs server for fast startup
  :config
  (unless (server-running-p)
    (server-start)))

(defalias 'list-buffers 'ibuffer)                ;; use ibuffer for buffer list



(provide 'init-packages)
