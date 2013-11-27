(progn
  (global-set-key (kbd "C-c f") 'find-file-in-project)

  ;; Font size
  (define-key global-map (kbd "C-+") 'text-scale-increase)
  (define-key global-map (kbd "C--") 'text-scale-decrease)

  ;; Use regex searches by default.
  (global-set-key (kbd "C-s") 'isearch-forward-regexp)
  (global-set-key (kbd "\C-r") 'isearch-backward-regexp)
  (global-set-key (kbd "C-M-s") 'isearch-forward)
  (global-set-key (kbd "C-M-r") 'isearch-backward)

  ;; useful commands
  (global-set-key (kbd "C-c r") 'revert-buffer)
  (windmove-default-keybindings) ;; Shift+direction
  (global-set-key (kbd "C-c x") 'execute-extended-command)
  (global-set-key (kbd "C-h a") 'apropos)  ;; Help should search more than just commands
  (global-set-key (kbd "C-c e") 'esk-eval-and-replace)  ;; Should be able to eval-and-replace anywhere.
  (global-set-key "\C-w" 'backward-kill-word)
  (global-set-key "\C-c\C-j" 'join-line)
  (global-set-key "\M- " 'just-one-space) ;; useful after joinline
  (global-set-key [f5] 'call-last-kbd-macro)
  (global-set-key (kbd "C-c g") 'magit-status)
  (global-set-key (kbd "C-c n") 'esk-cleanup-buffer)  ;; Perform general cleanup.
  (global-set-key (kbd "s-S") 'save-some-buffers) ;; save all

  
  (defun comment-sexp ()
    "Comment out the sexp at point."
    (interactive)
    (save-excursion
      (mark-sexp)
      (paredit-comment-dwim)))
  (global-set-key "\M-/" 'comment-sexp) 

  (defalias 'qrr 'query-replace-regexp)

  ;; This is a little hacky since VC doesn't support git add internally
  (eval-after-load 'vc
    (define-key vc-prefix-map "i"
      '(lambda () (interactive)
         (if (not (eq 'Git (vc-backend buffer-file-name)))
             (vc-register)
           (shell-command (format "git add %s" buffer-file-name))
           (message "Staged changes.")))))

  ;; Activate occur easily inside isearch
  (define-key isearch-mode-map (kbd "C-o")
    (lambda () (interactive)
      (let ((case-fold-search isearch-case-fold-search))
        (occur (if isearch-regexp isearch-string (regexp-quote isearch-string)))))))

(provide 'init-keybindings)
