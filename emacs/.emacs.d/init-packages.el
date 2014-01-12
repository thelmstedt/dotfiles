(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")


;; lazy install for el-get
(unless (require 'el-get nil t)
  (url-retrieve
   "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
   (lambda (s)
     (let (el-get-master-branch)
       (end-of-buffer)
       (eval-print-last-sexp)))))

(require 'el-get)

(setq el-get-sources
      '((:name deft
               :after (progn
                        (global-set-key [f1] 'deft)
                        (setq deft-directory "~/Dropbox/deft")
                        (setq deft-extension "org")
                        (setq deft-text-mode 'org-mode)
                        (add-hook 'deft-mode-hook 'deft-filter-clear t)
                        (define-key deft-mode-map (kbd "C-w") 'deft-filter-clear)))
        
        (:name magit
               :after (progn (global-set-key (kbd "C-x C-z") 'magit-status)))

        (:name paredit
               :after (progn
                        ;; stop ruby complaining about mismatched parens.. might screw with other modes
                        (defun gy-paredit-override-check-parens-interactively (condition)  t)
                        (setq paredit-override-check-parens-function 'gy-paredit-override-check-parens-interactively)
                        ;(define-key paredit-mode-map (kbd "C-w")
                        ;'paredit-backward-kill-word)
                        ))

        (:name autopair
               :after (progn
                        (autopair-global-mode)
                        (set-default 'autopair-dont-activate #'(lambda () (eq major-mode 'sldb-mode)))
                        (add-hook 'sldb-mode-hook #'(lambda () (setq autopair-dont-activate t)))))


        ;; TODO replace starter-kit-{list,js,ruby} with own code
        
        (:name rainbow-delimiters
               :after (progn
                        (require 'rainbow-delimiters)
                        (setq-default frame-background-mode 'dark)
                        (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
                        (add-hook 'octave-mode-hook 'rainbow-delimiters-mode)
                        (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)))

        ;; TODO replace redo+ with undotree

        (:name auto-complete
               :after (progn
                        (require 'auto-complete-config)
                        (ac-config-default)
                        (ac-flyspell-workaround)
                        (ac-linum-workaround)

                        (global-auto-complete-mode t)

                        ;; keys
                        (ac-set-trigger-key "TAB")
                        (define-key ac-completing-map (kbd "C-n") 'ac-next)
                        (define-key ac-completing-map (kbd "C-p") 'ac-previous)
                        (define-key ac-completing-map "\t" 'ac-complete)
                        (define-key ac-completing-map "\r" 'ac-complete)

                        ;; various tweaks
                        (setq ac-auto-start nil)
                        (setq ac-dwim t)
                        (setq ac-delay 0.3)
                        (setq ac-auto-show-menu t)

                        (setq ac-use-menu-map t)
                        
                        (setq ac-quick-help-delay 1)
                        (setq ac-quick-help-height 60)
                        (setq ac-fuzzy-enable t)

                        (set-default 'ac-sources
                                     '(ac-source-dictionary
                                       ac-source-words-in-buffer
                                       ac-source-words-in-same-mode-buffers
                                       ac-source-words-in-all-buffer))


                        ;; Exclude very large buffers from dabbrev
                        (defun smp-dabbrev-friend-buffer (other-buffer)
                          (< (buffer-size other-buffer) (* 1 1024 1024)))
                        
                        (setq dabbrev-friend-buffer-function 'smp-dabbrev-friend-buffer)))
        
        (:name ac-slime
               :after (progn
                        (require 'ac-slime)
                        (add-hook 'slime-mode-hook 'set-up-slime-ac)
                        (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
                        (add-hook 'slime-connected-hook 
                                  (lambda () 
                                    (define-key slime-mode-map (kbd "TAB") 'auto-complete) 
                                    (define-key slime-repl-mode-map (kbd "TAB") 'auto-complete)))))

        (:name ac-octave
               :after (progn
                        (defun ac-octave-mode-setup ()
                          (setq ac-sources '(ac-source-octave)))
                        (add-hook 'octave-mode-hook '(lambda () (ac-octave-mode-setup)))))

        (:name smex
               :after (progn
                        (global-set-key "\M-x" 'smex)
                        (global-set-key "\M-c" 'smex)
                        (global-set-key "\C-x\C-m" 'smex)
                        (global-set-key "\C-c\C-m" 'smex)))))

;; dired+ -  w - copy file name, F - open all marked files, A - regex search marked files
;; TODO investigate pinning org-mode to a tag - for now done manually to 7.9 
(setq my-packages
      (append
       '(el-get fuzzy dired+ haml-mode sass-mode color-theme rainbow-mode popup ido-ubiquitous color-theme-solarized org-mode haskell-mode)
       (mapcar 'el-get-source-name el-get-sources)))

(el-get 'sync my-packages)

(provide 'init-packages)
