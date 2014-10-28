; deft
(require 'deft)
(global-set-key [f1] 'deft)
(setq deft-directory "~/Dropbox/deft")
(setq deft-extension "org")
(setq deft-text-mode 'org-mode)
(add-hook 'deft-mode-hook 'deft-filter-clear t)
(define-key deft-mode-map (kbd "C-w") 'deft-filter-clear)

;; magit
(global-set-key (kbd "C-x C-z") 'magit-status)

;; rainbow-delimiters
(require 'rainbow-delimiters)
(setq-default frame-background-mode 'dark)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'octave-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'haskell-mode-hook 'rainbow-delimiters-mode)

;company-mode ;; TODO just turn it on for elisp/haskell/etc
(global-company-mode 1)

;undo-tree
(require 'undo-tree)
(global-undo-tree-mode 1)
(defalias 'redo 'undo-tree-redo)
(global-set-key (kbd "s-z") 'undo) 
(global-set-key (kbd "s-S-z") 'redo)

;;smex
(global-set-key "\M-x" 'smex)
(global-set-key "\M-c" 'smex)
(global-set-key "\C-x\C-m" 'smex)
(global-set-key "\C-c\C-m" 'smex)

;neotree
(require 'neotree)
(global-set-key (kbd "C-1") 'neotree-toggle)

;; requires find-file-in-project
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

; ido-mode
(ido-mode t)
(ido-ubiquitous t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10)


(defadvice ido-find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (and buffer-file-name
               (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; octave mode
;(autoload 'octave-mode "octave-mode" nil t)
(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))
(add-hook 'octave-mode-hook
          (lambda ()
            (abbrev-mode 1)
            (auto-fill-mode 1)
            (if (eq window-system 'x)
                (font-lock-mode 1))))


(setq auto-mode-alist
      (cons '("\\.zsh$" . sh-mode) auto-mode-alist))

;; paredit
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

(provide 'init-customisations)

