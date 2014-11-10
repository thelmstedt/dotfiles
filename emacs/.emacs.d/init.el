(defun ammend-path (p) 
  (progn 
    (setenv "PATH" (concat p ":" (getenv "PATH")))
    (push p exec-path)))

;;osx path shenanigans
(when (equal system-type 'darwin)
  (ammend-path "/usr/local/bin")
  (ammend-path "/Applications/ghc-7.8.3.app/Contents/bin")
  (ammend-path "/Users/tim/.cabal/bin"))


(ammend-path "~/.emacs-haskell/.cabal-sandbox/bin")

(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path (concat dotfiles-dir "/elisp"))

(require 'init-packages)
(require 'init-customisations)
(require 'init-keybindings)
(require 'init-org)
(require 'init-haskell)

(load-theme 'solarized-dark t)

(fset 'yes-or-no-p 'y-or-n-p)
(setq-default ispell-program-name "aspell")

;; backup/autosave settings
(defvar --backup-directory (concat user-emacs-directory "~/.emacs-backups"))
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
      auto-save-timeout 20              ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300)
      )



(setq
 linum-format "%d"
 initial-scratch-message "")

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
 '(menu-bar-mode nil)
 '(mouse-autoselect-window t)
 '(tool-bar-mode nil)
 '(visible-bell nil)
)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load "server")
(unless (server-running-p) (server-start))

;; :( osx
(when (equal system-type 'darwin)
  (require 'maxframe)
  (add-hook 'window-setup-hook 'maximize-frame t))
