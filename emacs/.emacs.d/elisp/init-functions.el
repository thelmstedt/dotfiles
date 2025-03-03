;;; init-functions.el --- Custom functions for my emacs config

;; word operations
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

;; buffer operations
(defvar killed-buffers '()
  "List of killed buffers for potential resurrection.")

(defun my-revert-buffer()
  "Revert buffer without asking for confirmation."
  (interactive)
  (revert-buffer t t))

(defun my-kill-buffer()
  "Kill current buffer without confirmation, appending to killed-buffers list."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffername (buffer-name)))
    (when filename
      (setq killed-buffers (cons filename killed-buffers)))
    (kill-buffer buffername)))

(defun my-unkill-buffer()
  "Reinstate the most recently killed buffer."
  (interactive)
  (let* ((last-killed (car killed-buffers))
         (remaining (cdr killed-buffers)))
    (message (format "unkilling buffer %s, state: %s" last-killed remaining))
    (setq killed-buffers remaining)
    (find-file last-killed)))

;; code operations
(defun comment-sexp ()
  "Comment out the sexp at point."
  (interactive)
  (save-excursion
    (mark-sexp)
    (paredit-comment-dwim)))

;; formatting operations
(defun align-use-package-comments (beg end)
  "Align trailing comments in use-package declarations from BEG to END.
Only aligns comments that appear after code, not lines that start with comments."
  (interactive "r")
  ;; regex breakdown:
  ;;   [^; \t\n]   - first char must NOT be semicolon or whitespace
  ;;   [^;\n]*     - followed by any chars except semicolons
  ;;   \\(\\s-*\\) - capture the spaces before comment
  ;;   ;; "        - match the actual comment start
  (align-regexp beg end "\\([^; \t\n][^;\n]*\\)\\(\\s-*\\) ;; " 2 1 t))

(defun align-buffer-comments ()
  "Align all trailing comments in current buffer."
  (interactive)
  (save-excursion
    (align-use-package-comments (point-min) (point-max))))

(provide 'init-functions)
;;; init-functions.el ends here
