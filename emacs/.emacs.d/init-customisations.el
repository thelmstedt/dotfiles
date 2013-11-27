;;; buffers
;;TODO - make this skip confirmation?
  
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

(fset 'yes-or-no-p 'y-or-n-p)

;; fixes graphical bug in osx
(linum-mode +1)
(setq linum-format "%d ")

;(require 'tramp)
                                        ;(setq tramp-default-method "ssh")

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

(autoload 'octave-mode "octave-mod" nil t)
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

(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.emacs-saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

(setq-default ispell-program-name "aspell")

(provide 'init-customisations)

