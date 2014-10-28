(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key "\C-cl" 'org-store-link)

(setq org-agenda-files '("~/Dropbox/deft"))

(setq org-agenda-include-diary t)
(setq org-agenda-include-all-todo t)
(setq org-hide-leading-stars t)
(setq org-startup-folded "showall")
(setq org-startup-indented "indent")


(setq org-startup-align-all-tables "align")
;(setq org-export-with-toc 4)
;(setq org-export-headline-levels 4)

(setq org-log-into-drawer t
      org-clock-modeline-total 'today
      org-clock-persist 'history)

(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!)" "CANCELED(c@!)")
        (sequence "WAITING(w@/!)")))

(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "LightSalmon" :weight bold))
        ("STARTED" . (:foreground "red"))
        ("WAITING" . (:foreground "red"))
        ("DONE" . (:foreground "forest green"))
        ("CANCELLED" . (:foreground "blue"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (org-todo (cond
             ((and (> n-done 0) (= n-not-done 0)) "DONE")
             ((and (> n-not-done 0) (= n-done 0)) "TODO")
             ((and (> n-not-done 0) (> n-done 0)) "STARTED")
             (t "TODO"))))

;; todo - this doesn't handle nested checkboxes - org-back-to-heading
;; http://lists.gnu.org/archive/html/emacs-orgmode/2011-06/msg00303.html
(add-hook 'org-checkbox-statistics-hook 'ndk/checkbox-list-complete)
(defun ndk/checkbox-list-complete ()
  (save-excursion
    (org-back-to-heading t)
    (let ((beg (point)) end
          org-log-done org-log-states) ; turn off logging
      (end-of-line)
      (setq end (point))
      (goto-char beg)
      ;; check for the cookie: [100%] or [N/N]
      (if (re-search-forward "\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]" end t)
          (if (match-end 1)
              (cond ((equal (match-string 1) "100%") (org-todo 'done))
                    ((equal (match-string 1) "0%") (org-todo "TODO"))
                    (t (org-todo "STARTED")))
            (cond ((and (> (match-end 2) (match-beginning 2))
                        (equal (match-string 2) (match-string 3)))
                   ;; all done - do the state change               
                   (org-todo 'done))
                  ((and (> (match-end 2) (match-beginning 2))
                        (equal (match-string 2) "0"))
                   (org-todo "TODO"))
                  (t (org-todo "STARTED"))))))))


(provide 'init-org)
