;; Treemacs
(global-set-key [f8] 'treemacs)
(global-set-key (kbd "C-c o p") 'treemacs)
(global-set-key (kbd "C-x p s") 'consult-ripgrep)

;; Projectile
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; fold this
;;(global-set-key (kbd "C-c C-f") 'fold-this-all)
(global-set-key (kbd "C-c C-f") 'fold-this)
(global-set-key (kbd "C-c M-f") 'fold-this-unfold-all)

;; multiple cursor
(global-set-key (kbd "C-<") 'mc/mark-next-like-this)
(global-set-key (kbd "C->") 'mc/mark-previous-like-this)

(define-key key-translation-map (kbd "M-S-d") (kbd "M-D"))
(global-set-key (kbd "M-D") 'mc/mark-next-like-this-word)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x 3") #'my/split-window-right-and-focus)
(global-set-key (kbd "C-x 2") #'my/split-window-below-and-focus)

;;compile
(global-set-key (kbd "C-c c c") 'compile)
(global-set-key (kbd "C-c c C") 'recompile)
(global-set-key (kbd "C-c p c c") 'projectile-compile-project)

(global-set-key [f10] 'recompile)


(keymap-set compilation-mode-map "C-<return>" 'compilation-next-error)

;; ace-window
(defun ace-window-golden-ratio()
	(interactive)
	 (ace-window nil)
	 (golden-ratio))
(global-set-key (kbd "C-x o") 'ace-window-golden-ratio)

(defun eshell-current-directory (&optional directory)
  "Open eshell current `default-directory' or DIRECTORY."
  (interactive)
  (let ((current-dir (or directory default-directory))
        (eshell-buffer (or (get-buffer "*eshell*")
                    (eshell))))
    (switch-to-buffer eshell-buffer)
    (eshell/cd current-dir)
    (eshell-next-prompt)
    ;; Regenerate prompt to show current directory.
    ;; Avoid sending any half written input commands
    (if (eobp)
        (eshell-send-input nil nil nil)
      (move-end-of-line nil)
      (eshell-kill-input)
      (eshell-send-input nil nil nil)
      (yank))))

(global-set-key (kbd "C-c o RET") 'eshell-current-directory)
(global-set-key (kbd "C-<return>") 'eshell-current-directory)

;;lsp
(global-set-key (kbd "C-c c d") 'lsp-goto-type-definition)

(keymap-set emacs-lisp-mode-map "C-c r" 'eval-buffer)

(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map [mouse-1] 'dired-find-alternate-file)))

(put 'dired-find-alternate-file 'disabled nil)

(global-set-key (kbd "C-h l") 'consult-theme)

(defun rc/duplicate-line ()
  "Duplicate current line"
  (interactive)
  (let ((column (- (point) (point-at-bol)))
        (line (let ((s (thing-at-point 'line t)))
                (if s (string-remove-suffix "\n" s) ""))))
    (move-end-of-line 1)
    (newline)
    (insert line)
    (move-beginning-of-line 1)
    (forward-char column)))


(global-set-key (kbd "C-.") 'rc/duplicate-line)

  (defun increment(amnt)
	(interactive)
 	(let ((bounds (bounds-of-thing-at-point 'number)))
		(if bounds
				(let* (
				 (beg (car bounds))
				 (end (cdr bounds))
				 (deleted (delete-and-extract-region beg end)))
		(insert (format "%s" ( + amnt (string-to-number deleted))))))
))

(global-set-key (kbd "C-S-i")
                (lambda () (interactive) (increment 1)))
(global-set-key (kbd "C-M-S-i")
                (lambda () (interactive) (increment -1)))
