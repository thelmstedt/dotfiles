
(use-package general
  :config

  (general-create-definer my/leader-keys
    :prefix "C-c")

  (general-create-definer my/local-leader
    :prefix "C-c l")

  ;; global bindings including consult
  (general-define-key
   ;; core consult bindings
   "M-x" '(execute-extended-command :which-key "M-x")
   "C-x B  (general-evil-setup)" '(consult-buffer :which-key "switch buffer")
   "C-s" '(consult-line :which-key "search")
   "M-y" '(consult-yank-pop :which-key "yank ring")

   ;; existing bindings
   "C-+" '(text-scale-increase :which-key "zoom in")
   "C--" '(text-scale-decrease :which-key "zoom out")
   "C-w" '(backward-delete-word :which-key "kill word")
   "M-/" '(comment-sexp :which-key "comment")
   "s-w" '(my-kill-buffer :which-key "kill buffer")
   "s-W" '(my-unkill-buffer :which-key "unkill buffer")
   "s-r" '(my-revert-buffer :which-key "revert buffer")
   "s-S" '(save-some-buffers :which-key "save all")

   ;; window movement
   "s-h" '(windmove-left :which-key "window left")
   "s-j" '(windmove-down :which-key "window down")
   "s-k" '(windmove-up :which-key "window up")
   "s-l" '(windmove-right :which-key "window right"))

  ;; leader key bindings including consult
  (my/leader-keys
   "f" '(consult-find :which-key "find file")
   "g" '(consult-ripgrep :which-key "ripgrep")
   "x" '(execute-extended-command :which-key "M-x")
   "r" '(revert-buffer :which-key "revert")
   "m" '(magit-status :which-key "magit"))

  ;; goto menu with consult
  (general-def
    :prefix "M-g"
    "g" '(consult-goto-line :which-key "goto line")
    "i" '(consult-imenu :which-key "imenu"))

  ;; minibuffer maps including consult
  (general-def
    :keymaps '(minibuffer-local-map
               minibuffer-local-ns-map
               minibuffer-local-completion-map
               minibuffer-local-must-match-map)
    "M-s" 'consult-history
    "M-r" 'consult-history
    "C-w" 'backward-kill-word)

  ;; isearch integration with consult
  (general-def
    :keymaps 'isearch-mode-map
    "M-e" 'consult-isearch-history
    "C-o" (lambda ()
            (interactive)
            (let ((case-fold-search isearch-case-fold-search))
              (occur (if isearch-regexp
                        isearch-string
                        (regexp-quote isearch-string))))))

  ;; vc stuff
  (general-def
    :keymaps 'vc-prefix-map
    "i" (lambda ()
          (interactive)
          (if (not (eq 'Git (vc-backend buffer-file-name)))
              (vc-register)
            (shell-command (format "git add %s" buffer-file-name))
            (message "Staged changes.")))))

(defalias 'qrr 'query-replace-regexp)

(provide 'init-keybindings)
