(defvar prelude-dir (file-name-directory load-file-name))

;; path helper
(defun amend-path (p)
  (progn
    (setenv "PATH" (concat p ":" (getenv "PATH")))
    (push p exec-path)))

(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path (concat dotfiles-dir "/elisp"))

;; ripgrep setup
(when (executable-find "rg")
  (setenv "RIPGREP_CONFIG_PATH"
          (expand-file-name "~/.ripgreprc")))

;; package setup
(require 'package)
(if (eq system-type 'windows-nt)
    (add-to-list 'package-archives
                 '("melpa" . "http://melpa.org/packages/") t)
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/") t))

(setq package-user-dir (expand-file-name "elpa" prelude-dir))
(package-initialize)

;; setup use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)
(setq use-package-always-ensure t)

;; load configs
(require 'init-packages)
(require 'init-functions)
(require 'init-keybindings)
(require 'init-org)
(require 'init-ibuffer)


;; general settings
(setq initial-major-mode 'org-mode)
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default ispell-program-name "aspell")

;; backup settings
(defvar --backup-directory (concat user-emacs-directory ".emacs-backups"))
(if (not (file-exists-p --backup-directory))
    (make-directory --backup-directory t))
(setq backup-directory-alist `(("." . ,--backup-directory)))
(setq make-backup-files t
      backup-by-copying t
      version-control t
      delete-old-versions t
      delete-by-moving-to-trash t
      kept-old-versions 6
      kept-new-versions 9
      auto-save-default t
      auto-save-timeout 30
      auto-save-interval 300
      auto-save-no-message t)

;; undo tree settings
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

;; auto sudo edit when needed
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t))
      enable-local-variables :safe)

;; clean trailing whitespace except in org mode
(add-hook 'before-save-hook
          #'(lambda ()
              (unless (eq major-mode 'org-mode)
                (delete-trailing-whitespace))))

;; custom settings - kept in init.el for visibility
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(fill-column 120)
 '(global-display-line-numbers-mode 1)
 '(global-visual-line-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-scratch-message "")
 '(menu-bar-mode nil)
 '(mouse-autoselect-window t)
 '(org-agenda-files nil nil nil "Customized with use-package org")
 '(package-selected-packages nil)
 '(tab-width 2)
 '(tool-bar-mode nil)
 '(treesit-simple-indent-rules nil t)
 '(visible-bell nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
