(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")))

(setq package-list '(haskell-mode
                     shm
                     company
                     company-ghc
                     ghc

                     undo-tree
                     smex
                     neotree
                     paredit
                     fuzzy
                     dired+
                     color-theme
                     markdown-mode

                     ;;rainbow-mode
                     popup
                     ido-ubiquitous
                     color-theme-solarized


                     find-file-in-project
                     deft
                     magit
                     git-commit-mode
                     git-rebase-mode
                     rainbow-delimiters
                     ibuffer-vc fullframe

                     ;; just for osx
                     maxframe

                     ;;todo can we have this installed from orgmode automatically? currently just skips as it's included
                     org))

; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(defun package-list-unaccounted-packages ()
  "Like `package-list-packages', but shows only the packages that
  are installed and are not in `package-list'.  Useful for
  cleaning out unwanted packages."
  (interactive)
  (package-show-package-list
   (remove-if-not (lambda (x) (and (not (memq x package-list))
                            (not (package-built-in-p x))
                            (package-installed-p x)))
                  (mapcar 'car package-archive-contents))))

(provide 'init-packages)
