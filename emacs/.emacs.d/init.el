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
(add-to-list 'load-path dotfiles-dir)

(require 'init-packages)
(require 'init-customisations)
(require 'init-keybindings)
(require 'init-org)
(require 'init-haskell)

(load-theme 'solarized-dark t)

(fset 'yes-or-no-p 'y-or-n-p)
(setq-default ispell-program-name "aspell")

(setq
   backup-by-copying t ; don't clobber symlinks
   backup-directory-alist '(("." . "~/.emacs-saves")) ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t  ; use versioned backups
   linum-format "%d")

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
