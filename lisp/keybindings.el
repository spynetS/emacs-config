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

;;compile
(global-set-key (kbd "C-c c c") 'compile)
(global-set-key (kbd "C-c c C") 'recompile)
(global-set-key (kbd "C-c p c c") 'projectile-compile-project)
(global-set-key [f10] 'recompile)

;; ace-window
(global-set-key (kbd "C-x o") 'ace-window)

;;lsp
(global-set-key (kbd "C-c c d") 'lsp-goto-type-definition)

;; For single click with mouse-1
(define-key dired-mode-map [mouse-1] 'dired-mouse-find-file)
;; For double click with mouse-2 (optional)
(define-key dired-mode-map [double-mouse-1] 'dired-mouse-find-file)
