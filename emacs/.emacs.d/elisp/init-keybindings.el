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
(global-set-key "\C-w" 'backward-delete-word)
(global-set-key "\C-c\C-j" 'join-line)
(global-set-key "\M- " 'just-one-space) ;; useful after joinline
(global-set-key [f5] 'call-last-kbd-macro)
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-c n") 'esk-cleanup-buffer)  ;; Perform general cleanup.
(global-set-key (kbd "s-S") 'save-some-buffers) ;; save all


(let ((map minibuffer-local-map))
  (define-key map "\C-w"   'backward-delete-word)
)


(defun delete-word (arg)
"Delete characters forward until encountering the end of a word.
With argument, do this that many times."
(interactive "p")
(delete-region (point) (progn (forward-word arg) (point))))

(defun backward-delete-word (arg)
"Delete characters backward until encountering the end of a word.
With argument, do this that many times."
(interactive "p")
(delete-word (- arg)))

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
      (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))


;;; buffers
(defun my-revert-buffer()
  "revert buffer without asking for confirmation"
  (interactive "")
  (revert-buffer t t))
(global-set-key (kbd "s-r") 'my-revert-buffer)

(defvar killed-buffers '())

(defun my-kill-buffer()
  "kills the current buffer without confirmation, appending it to a list of killed buffers"
  (interactive)

  (let ((filename (buffer-file-name))
        (buffername (buffer-name)))
    (if filename
        (setq killed-buffers (append (list filename) killed-buffers)))
    (kill-buffer buffername)))
(global-set-key (kbd "s-w") 'my-kill-buffer)

(defun my-unkill-buffer()
  "reinstates the last killed buffer, removing all instances of it from the list of killed buffers"
  (interactive)
  (let* ((last-killed (first killed-buffers))
         (remaining (remq last-killed killed-buffers)))
    (message (format "unkilling buffer %s, state: %s" last-killed remaining))
    (setq killed-buffers remaining)
    (find-file last-killed)))

(global-set-key (kbd "s-W") 'my-unkill-buffer)



(provide 'init-keybindings)
