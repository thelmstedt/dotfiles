(use-package org
  :mode ("\\.org$" . org-mode)
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda))
  :custom
  (org-hide-leading-stars t)
  (org-src-fontify-natively t)
  (org-src-preserve-indentation t)
  (org-startup-folded "showall")
  (org-startup-indented "indent")
  (org-startup-align-all-tables "align")
  (org-log-into-drawer t)
  (org-clock-modeline-total 'today)
  (org-clock-persist 'history)
  (org-log-done t)
  (org-todo-keywords '((sequence "TODO" "INPROGRESS" "DONE")))
  (org-todo-keyword-faces '(("INPROGRESS" . (:foreground "blue" :weight bold))))
  (org-agenda-files '("~/Dropbox/deft"))
  (org-agenda-include-diary t)
  (org-agenda-include-all-todo t)
  (org-agenda-show-log t)
  :config
  (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
  (add-hook 'org-checkbox-statistics-hook 'ndk/checkbox-list-complete))

;; helper functions
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (cond ((and (> n-done 0) (= n-not-done 0)) "DONE")
                    ((and (> n-not-done 0) (= n-done 0)) "TODO")
                    ((and (> n-not-done 0) (> n-done 0)) "INPROGRESS")
                    (t "TODO")))))

(defun ndk/checkbox-list-complete ()
  "Update parent TODO based on checkbox statistics"
  (save-excursion
    (org-back-to-heading t)
    (let ((beg (point)) end)
      (end-of-line)
      (setq end (point))
      (goto-char beg)
      (when (re-search-forward "\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]" end t)
        (cond ((match-end 1)
               (match-string 1)
               (cond ((equal (match-string 1) "100%") (org-todo 'done))
                     ((equal (match-string 1) "0%") (org-todo "TODO"))
                     (t (org-todo "INPROGRESS"))))
              ((and (> (match-end 2) (match-beginning 2))
                    (equal (match-string 2) (match-string 3)))
               (org-todo 'done))
              ((and (> (match-end 2) (match-beginning 2))
                    (equal (match-string 2) "0"))
               (org-todo "TODO"))
              (t (org-todo "INPROGRESS")))))))

(defun yeet-done-to-archive ()
  (interactive)
  (save-excursion
    ;; grab current month from cursor position
    (let ((month (org-get-heading t t t t)))
      ;; cut all DONE entries
      (org-map-entries
       (lambda ()
         (when (string-match "DONE" (org-get-heading t t t t))
           (org-cut-subtree)))
       nil 'tree)
      ;; find/create Archive/month section and paste
      (goto-char (point-min))
      (unless (re-search-forward "^* Archive" nil t)
        (goto-char (point-max))
        (insert "\n* Archive"))
      (unless (re-search-forward (concat "^** " month) nil t)
        (insert (concat "\n** " month)))
      (forward-line)
      (org-paste-subtree))))


(provide 'init-org)
