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
 '(menu-bar-mode nil)
 '(mouse-autoselect-window t)
 '(tool-bar-mode nil)
 '(visible-bell nil)
 '(initial-scratch-message "")
)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load "server")
(unless (server-running-p) (server-start))