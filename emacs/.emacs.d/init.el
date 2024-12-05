(defvar prelude-dir (file-name-directory load-file-name))

(defun amend-path (p)
  (progn
    (setenv "PATH" (concat p ":" (getenv "PATH")))
    (push p exec-path)))

(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path (concat dotfiles-dir "/elisp"))


(if (fboundp 'with-eval-after-load)
    (defalias 'after-load 'with-eval-after-load)
  (defmacro after-load (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))


(require 'init-packages)
(require 'init-customisations)
(require 'init-keybindings)
(require 'init-org)
(require 'init-ibuffer)

(load-theme 'zenburn t)
(setq initial-major-mode 'org-mode)

(fset 'yes-or-no-p 'y-or-n-p)
(setq-default ispell-program-name "aspell")

(defvar --backup-directory (concat user-emacs-directory ".emacs-backups"))
(if (not (file-exists-p --backup-directory))
        (make-directory --backup-directory t))
(setq backup-directory-alist `(("." . ,--backup-directory)))
(setq make-backup-files t               ; backup of a file the first time it is saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 6               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 9               ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t               ; auto-save every buffer that visits a file
      auto-save-timeout 30              ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 300            ; number of keystrokes between auto-saves (default: 300)
      )

(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))


(windmove-default-keybindings 's)




(add-hook 'before-save-hook
          (lambda ()
            (unless (eq major-mode 'org-mode)
              (delete-trailing-whitespace))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(fill-column 120)
 '(global-linum-mode 1)
 '(global-visual-line-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-scratch-message "")
 '(menu-bar-mode nil)
 '(mouse-autoselect-window t)
 '(package-selected-packages
   '(haskell-mode yaml-mode json-mode zop-to-char zenburn-theme which-key volatile-highlights undo-tree super-save smex smartrep smartparens rainbow-delimiters projectile popup paredit operate-on-number nlinum neotree move-text markdown-mode magit imenu-anywhere ido-completing-read+ ibuffer-vc hl-todo guru-mode git-timemachine git-modes gist fuzzy fullframe flycheck find-file-in-project expand-region editorconfig easy-kill discover-my-major diminish diff-hl deft crux browse-kill-ring anzu ag))
 '(tool-bar-mode nil)
 '(visible-bell nil)
 '(windmove-default-keybindings 's))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load "server")
(unless (server-running-p) (server-start))
