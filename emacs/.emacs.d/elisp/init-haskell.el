; HASKELL-MODE
; ------------

; Choose indentation mode
;; Use haskell-mode indentation
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;; Use structured-haskell-mode
;(add-hook 'haskell-mode-hook 'structured-haskell-mode)

; Add F8 key combination for going to imports block
(eval-after-load 'haskell-mode
  '(define-key haskell-mode-map [f8] 'haskell-navigate-imports))

(custom-set-variables
 ; Set up hasktags (part 2)
 '(haskell-tags-on-save t)
 ; Set up interactive mode (part 2)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 ; Set interpreter to be "cabal repl"
 '(haskell-process-type 'cabal-repl))

; Add key combinations for interactive haskell-mode
(eval-after-load 'haskell-mode '(progn
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
  (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
  (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
  (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
  (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)
  (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)))
(eval-after-load 'haskell-cabal '(progn
  (define-key haskell-cabal-mode-map (kbd "C-`") 'haskell-interactive-bring)
  (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-ode-clear)
  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))

; GHC-MOD
; -------

(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))

; COMPANY-GHC
; -----------

; Enable company-mode
(require 'company)
; Use company in Haskell buffers
(add-hook 'haskell-mode-hook 'company-mode)
; Use company in all buffers
;;(add-hook 'after-init-hook 'global-company-mode)

(add-to-list 'company-backends 'company-ghc)
(custom-set-variables '(company-ghc-show-info t))


(require 'cl-lib)
(require 'thingatpt)

(defun haskell-process-completions-at-point ()
  "A company-mode-compatible complete-at-point function."
  (-when-let (process (haskell-process))
    (-when-let (symbol (symbol-at-point))
      (destructuring-bind (start . end) (bounds-of-thing-at-point 'symbol)
        (let ((completions (haskell-process-get-repl-completions (haskell-process)
                                                                 (symbol-name symbol))))
          (list start end completions))))))

(add-hook 'haskell-mode-hook
          (lambda ()
            (setq-local completion-at-point-functions
                        '(haskell-process-completions-at-point))))


(provide 'init-haskell)
