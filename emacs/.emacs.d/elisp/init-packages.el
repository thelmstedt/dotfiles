; list the packages you want
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

;;                     rainbow-mode 
                     popup 
                     ido-ubiquitous 
                     color-theme-solarized 
                     org ;;todo can we have this installed from orgmode automatically? currently just skips as it's included
                     find-file-in-project
                     deft 
                     magit 
                     rainbow-delimiters 
                     ))

; list the repositories containing them
(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")))

; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(provide 'init-packages)
