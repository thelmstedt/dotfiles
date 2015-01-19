(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; keybindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)


;; general
(setq org-hide-leading-stars t
      org-startup-folded "showall"
      org-startup-indented "indent"
      org-startup-align-all-tables "align"
      org-log-into-drawer t
      org-clock-modeline-total 'today
      org-clock-persist 'history)

;; todos
(setq org-log-done t
      org-todo-keywords '((sequence "TODO" "INPROGRESS" "DONE"))
      org-todo-keyword-faces '(("INPROGRESS" . (:foreground "blue" :weight bold))))

;; habits
(require 'org)
(require 'org-install)
(require 'org-habit)
(add-to-list 'org-modules "org-habit")
(setq org-habit-preceding-days 14
      org-habit-following-days 1
      org-habit-graph-column 40
      org-habit-show-habits-only-for-today t
      org-habit-show-all-today t)

;; agenda
(setq org-agenda-files '("~/Dropbox/deft")
      org-agenda-include-diary t
      org-agenda-include-all-todo t
      org-agenda-show-log t)

;; export
;; (setq org-export-with-toc 4
;;       org-export-headline-levels 4)



(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (org-todo (cond
             ((and (> n-done 0) (= n-not-done 0)) "DONE")
             ((and (> n-not-done 0) (= n-done 0)) "TODO")
             ((and (> n-not-done 0) (> n-done 0)) "INPROGRESS")
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

;; babel
(require 'ob)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((sh . t)
   (haskell . t)))

(setq org-src-fontify-natively t
      org-confirm-babel-evaluate nil)

(add-hook 'org-babel-after-execute-hook
          (lambda ()
            (condition-case nil
                (org-display-inline-images)
              (error nil)))
          'append)



(provide 'init-org)
