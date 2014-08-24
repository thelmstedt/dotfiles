(require 'package)
;; (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")


;; lazy install for el-get
(unless (require 'el-get nil t)
  (progn
    (url-retrieve
     "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
     (lambda (s)
       (let (el-get-master-branch)
         (end-of-buffer)
         (eval-print-last-sexp))))
    (require 'el-get)))

(require 'el-get)

(setq el-get-sources
      '((:name deft
               :post-init (progn
                        (global-set-key [f1] 'deft)
                        (setq deft-directory "~/Dropbox/deft")
                        (setq deft-extension "org")
                        (setq deft-text-mode 'org-mode)
                        (add-hook 'deft-mode-hook 'deft-filter-clear t)
                        (define-key deft-mode-map (kbd "C-w") 'deft-filter-clear)))
        
        (:name magit
               :post-init (progn (global-set-key (kbd "C-x C-z") 'magit-status)))

        
        (:name rainbow-delimiters
               :post-init (progn
                        (require 'rainbow-delimiters)
                        (setq-default frame-background-mode 'dark)
                        (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
                        (add-hook 'octave-mode-hook 'rainbow-delimiters-mode)
                        (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)))

        (:name haskell-mode
               :post-init
               (progn
                 (require 'haskell-mode-autoloads)
                 (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
                 (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
                 (setq haskell-process-type 'cabal-repl haskell-process-args-cabal-repl '("--ghc-option=-ferror-spans"))
                 (define-key haskell-mode-map (kbd "C-x C-d") nil)
                 (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
                 (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
                 (define-key haskell-mode-map (kbd "C-c C-b") 'haskell-interactive-switch)
                 (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
                 (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
                 (define-key haskell-mode-map (kbd "C-c M-.") nil)
                 (define-key haskell-mode-map (kbd "C-c C-d") nil)
                 (define-key haskell-mode-map (kbd "C-c v c") 'haskell-cabal-visit-file)))

        (:name structured-haskell-mode
               :depends haskell-mode
               :type github
               :pkgname "chrisdone/structured-haskell-mode"
               :features shm
               :load-path "elisp")

        (:name company-mode
               :post-init (progn 
                            (global-company-mode 1)
                            ))
        
        ;;TODO this would be much nicer to do directly from MELPA
        ;; debug with company-ghc-diagnose
        (:name company-ghc
               :depends (company-mode ghc-mod) ;;ghc-mod needs to be aliased to ghc
	       :type github
	       :pkgname "iquiw/company-ghc"
	       :features company-ghc
               :post-init 
               (progn
                 (add-to-list 'company-backends 'company-ghc)
                 ))

        ;; if this breaks - check the version installed matches the one defined in ~/.emacs-haskell/emacs-haskell.cabal
        ;; debug with ghc-debug
        (:name ghc-mod
               :features ghc
               :post-init
               (progn
                 (autoload 'ghc-init "ghc" nil t)
                 (autoload 'ghc-debug "ghc" nil t)
                 (defun el-get-ghc-mod-hook ()
                   (ghc-init)
                   (flymake-mode))
                 (add-hook 'haskell-mode-hook 'el-get-ghc-mod-hook)))

        (:name undo-tree
               :post-init 
               (progn
                 (require 'undo-tree)
                 (global-undo-tree-mode 1)

                 (defalias 'redo 'undo-tree-redo)
                 (global-set-key (kbd "s-z") 'undo) 
                 (global-set-key (kbd "s-S-z") 'redo)))
        
        (:name smex
               :post-init (progn
                        (global-set-key "\M-x" 'smex)
                        (global-set-key "\M-c" 'smex)
                        (global-set-key "\C-x\C-m" 'smex)
                        (global-set-key "\C-c\C-m" 'smex)))
        (:name neotree
               :post-init 
               (progn
                 (require 'neotree)
                 (global-set-key [f8] 'neotree-toggle)

                 (defun neotree-project-dir ()
                   "Open dirtree using the git root."

                   (require 'find-file-in-project)

                   (interactive)
                   (let ((project-dir (ffip-project-root))
                         (file-name (buffer-file-name)))
                     (if project-dir
                         (progn
                           (neotree-dir project-dir)
                           (neotree-find file-name))
                       (message "Could not find git project root."))))

                 (global-set-key (kbd "C-c C-p") 'neotree-project-dir)
                 

                 ))
))

;; dired+ -  w - copy file name, F - open all marked files, A - regex search marked files


(setq my-packages
      (append
       '(el-get
         paredit 
         fuzzy 
         dired+ 
         color-theme 
         rainbow-mode 
         popup 
         ido-ubiquitous 
         color-theme-solarized 
         org-mode
         find-file-in-project
         )

       (mapcar 'el-get-source-name el-get-sources)))

(el-get 'sync my-packages)

(provide 'init-packages)
