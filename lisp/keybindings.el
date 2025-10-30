;; Treemacs
(global-set-key [f8] 'treemacs)
(global-set-key (kbd "C-c o p") 'treemacs)

;; Projectile
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; fold this
(global-set-key (kbd "C-c C-f") 'fold-this-all)
(global-set-key (kbd "C-c C-F") 'fold-this)
(global-set-key (kbd "C-c M-f") 'fold-this-unfold-all)

;; multiple cursor
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
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

;; ace-window
(global-set-key (kbd "C-x o") 'ace-window)

;;lsp
(global-set-key (kbd "C-c c d") 'lsp-goto-type-definition)

(keymap-set emacs-lisp-mode-map "C-c r" 'eval-buffer)

(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map [mouse-1] 'dired-find-alternate-file)))

(put 'dired-find-alternate-file 'disabled nil)

  (global-set-key (kbd "C-h l") 'load-theme)
