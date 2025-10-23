(setq inhibit-startup-message t  ; Don't show the splash screen
      visible-bell t)            ; Flash when the bell rings

;; Disable unnecessary UI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq display-line-numbers 'relative)
(global-display-line-numbers-mode 1)
(global-hl-line-mode +1)          ;; highlight current line

;;(setq-default indent-tabs-mode nil) ;; use spaces hehe
;;(setq-default tab-width 4)
(setq-default indent-tabs-mode t)
(setq-default tab-width 2) ; Assuming you want your tabs to be two spaces wide

(fset 'yes-or-no-p 'y-or-n-p)

(setq auto-save-default nil)
;; Optional: Change auto-save file location (to avoid clutter)
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.config/emacs/auto-saves/") t)))

(setq-default cursor-type 'bar)  ; Slim vertical bar cursor
(blink-cursor-mode 1)
(setq cursor-in-non-selected-windows nil)

(smartparens-global-mode t)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(delete-selection-mode 1)

;; Enable automatic closing of tags
(setq web-mode-enable-auto-closing t)
;; Enable automatic pair insertion (like <div></div>)
(setq web-mode-enable-auto-pairing t)

(setq custom-safe-themes t)
(load-theme 'doom-dark+)

;; dired
(setq dired-listing-switches "-lah --group-directories-first")

(dashboard-open)

(defun jetbrains ()
  "Starts jetbrains style"
  (interactive)
  (load-theme 'jetbrains-darcula t)
  (projectile-switch-project)
  (treemacs))
