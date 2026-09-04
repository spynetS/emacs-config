;; Load custom Emacs Lisp files -*- lexical-binding: t; -*-

(add-to-list 'load-path "/home/spy/.config/emacs/lisp")
(setq browse-url-browser-function 'browse-url-firefox)

;; Java Configuration
(setenv "JAVA_HOME" "/usr/lib/jvm/default")
(setq exec-path (cons (concat (getenv "JAVA_HOME") "/bin") exec-path))

;; Disable auto-save files
(setq auto-save-default nil
      auto-save-list-file-prefix nil)

;; Disable backup files
(setq make-backup-files nil)

;; Disable lockfiles
(setq create-lockfiles nil)

;; Disable desktop save
(setq desktop-save nil)

;; LSP performance optimization
(setq gc-cons-threshold 100000000) ;; 100 MB

(setq password-cache t)
(setq password-cache-expiry 3600) ;; 1 hour

(setq truncate-lines t)

;; Load configuration modules
(load "secret")

;; Custom file configuration
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(defun sync-timeedit ()
  (let ((url "https://cloud.timeedit.net/ju/web/open/ri607QeQ5q2Zb9Q5m68rn3Q6yZZ081Q1ZZQYcbu.ics")
        (file "~/org/timeedit.ics"))
    (url-copy-file url file t)
    (message "TimeEdit calendar synced!")))

;; Run sync whenever Emacs starts
(add-hook 'after-init-hook #'my/sync-timeedit)

(defun my/sync-timeedit ()
  (interactive)
  (url-copy-file "https://cloud.timeedit.net/ju/web/open/ri607QeQ5q2Zb9Q5m68rn3Q6yZZ081Q1ZZQYcbu.ics" 
                 "~/org/timeedit.ics" t)
  (message "TimeEdit synced!"))

;; Add the file to your agenda list
;;(add-to-list 'org-agenda-files "~/org/timeedit.ics")

(require 'icalendar)
(defun my/import-timeedit-to-calendar ()
  (interactive)
  (icalendar-import-file "~/org/timeedit.ics" "~/org/diary"))

(setq diary-file "~/org/diary") ;; Or wherever you keep your
(setq mark-diary-entries-in-calendar t)
(add-hook 'diary-display-hook 'diary-fancy-display)

(require 'package)

;; Add package archives
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu"   . "https://elpa.gnu.org/packages/")
        ("org"   . "https://orgmode.org/elpa/")))

;; Initialize the package system
(package-initialize)

;; Refresh package contents if needed
(unless package-archive-contents
  (package-refresh-contents))

;; Install use-package if not already installed
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package magit
  :ensure t
  :defer t
  :bind (("C-x g" . magit-status)))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.5))

;; Enable Vertico
(use-package vertico
  :init
  (vertico-mode 1))

(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Persist history over Emacs restarts
(use-package savehist
  :init
  (savehist-mode 1))

;; Emacs minibuffer configurations
(setq enable-recursive-minibuffers t
      read-extended-command-predicate #'command-completion-default-include-p
      minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))

;; Optional: Enable context menu mode
(context-menu-mode 1)

(use-package consult
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer)
         ("M-y" . consult-yank-pop)))

(use-package embark
  :bind (("M-o" . embark-act)
         ("C-h B" . embark-bindings))
  :init (setq prefix-help-command #'embark-prefix-help-command))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package fold-this)
(use-package projectile)

(use-package web-mode
  :ensure t
  :mode
  (("\\.phtml\\'" . web-mode)
   ("\\.php\\'" . web-mode)
   ("\\.ts\\'" . web-mode)
   ("\\.tsx\\'" . web-mode)
   ("\\.tpl\\'" . web-mode)
   ("\\.[agj]sp\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.mustache\\'" . web-mode)
   ("\\.djhtml\\'" . web-mode)))

(use-package lsp-mode
  :commands lsp)

;; (use-package lsp-mode
;;     :commands lsp
;;     :config
;;     (setq lsp-prefer-flymake nil   ;; Use flycheck instead of flymake
;;           lsp-enable-snippet t
;;           lsp-completion-provider :capf))

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-code-actions t))

(use-package company
  :config
	(global-company-mode 1)
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0))

(use-package java-snippets)

(use-package all-the-icons :if (display-graphic-p))
(use-package doom-themes)
(use-package kanagawa-themes)
(use-package adwaita-dark-theme)
(use-package batppuccin)
(use-package dimmer
  :init
  (dimmer-mode))
(use-package beacon
  :init
  (beacon-mode))
;; (use-package nyan-mode
;;   :init
;;   (nyan-mode)
;;   (nyan-start-animation))
;; (use-package parrot
;;   :hook (compilation)
;;   :init
;;   (parrot-mode))

(use-package doom-modeline
  :init
  (doom-modeline-mode)
  :custom
  ;; Optional tweaks
  (doom-modeline-height 18)
  (doom-modeline-bar-width 4)
  (doom-modeline-battery-status t)
  (doom-modeline-buffer-file-name-style 'truncate-with-project)
  (doom-modeline-minor-modes nil)
	(doom-modeline-enable-word-count t)
  (doom-modeline-time-analogue-clock t)
  (doom-modeline-indent-info t)
	(doom-modeline-major-mode-icon t)
  (doom-modeline-icon t))  ;; requires all-the-icons package

(use-package multiple-cursors)

(use-package evil-numbers
  :config
  (global-set-key (kbd "C-c +") 'evil-numbers/inc-at-pt)
  (global-set-key (kbd "C-c =") 'evil-numbers/inc-at-pt)
  (global-set-key (kbd "C-c -") 'evil-numbers/dec-at-pt)
  (global-set-key (kbd "C-c C-+") 'evil-numbers/inc-at-pt-incremental)
  (global-set-key (kbd "C-c C-=") 'evil-numbers/inc-at-pt-incremental)
  (global-set-key (kbd "C-c C--") 'evil-numbers/dec-at-pt-incremental))
1
(use-package eyebrowse
  :ensure t
  :config
  ;; Enable eyebrowse
  (eyebrowse-mode t)

  ;; Optional: start with a specific number of workspaces
  (setq eyebrowse-new-workspace t)

  ;; Keybindings for quick workspace switching
  (global-set-key (kbd "M-1") 'eyebrowse-switch-to-window-config-1)
  (global-set-key (kbd "M-2") 'eyebrowse-switch-to-window-config-2)
  (global-set-key (kbd "M-3") 'eyebrowse-switch-to-window-config-3)
  (global-set-key (kbd "M-4") 'eyebrowse-switch-to-window-config-4)
  (global-set-key (kbd "M-0") 'eyebrowse-last-window-config)

  ;; Optional: customize the modeline indicator
  (setq eyebrowse-mode-line-separator " | ")
  (setq eyebrowse-new-workspace t)
  (setq eyebrowse-mode-line-left-delimiter "[")
  (setq eyebrowse-mode-line-right-delimiter "]")
  (setq eyebrowse-mode-line-style 'smart))

(use-package jtsx
  :ensure t
  :mode (("\\.jsx?\\'" . jtsx-jsx-mode)
         ("\\.tsx\\'" . jtsx-tsx-mode))

  :commands jtsx-install-treesit-language
  :hook ((jtsx-jsx-mode . hs-minor-mode)
         (jtsx-tsx-mode . hs-minor-mode)
         (jtsx-typescript-mode . hs-minor-mode))
  :custom
  ;; Optional customizations
  (js-indent-level 4)
  ;; (typescript-ts-mode-indent-offset 2)
  ;; (jtsx-switch-indent-offset 0)
  ;; (jtsx-indent-statement-block-regarding-standalone-parent nil)
  ;; (jtsx-jsx-element-move-allow-step-out t)
  ;; (jtsx-enable-jsx-electric-closing-element t)
  ;; (jtsx-enable-electric-open-newline-between-jsx-element-tags t)
  ;; (jtsx-enable-jsx-element-tags-auto-sync nil)
  ;; (jtsx-enable-all-syntax-highlighting-features t)
  :config
  (defun jtsx-bind-keys-to-mode-map (mode-map)
    "Bind keys to MODE-MAP."
    (define-key mode-map (kbd "C-c C-j") 'jtsx-jump-jsx-element-tag-dwim)
    (define-key mode-map (kbd "C-c j o") 'jtsx-jump-jsx-opening-tag)
    (define-key mode-map (kbd "C-c j c") 'jtsx-jump-jsx-closing-tag)
    (define-key mode-map (kbd "C-c j r") 'jtsx-rename-jsx-element)
    (define-key mode-map (kbd "C-c <down>") 'jtsx-move-jsx-element-tag-forward)
    (define-key mode-map (kbd "C-c <up>") 'jtsx-move-jsx-element-tag-backward)
    (define-key mode-map (kbd "C-c C-<down>") 'jtsx-move-jsx-element-forward)
    (define-key mode-map (kbd "C-c C-<up>") 'jtsx-move-jsx-element-backward)
    (define-key mode-map (kbd "C-c C-S-<down>") 'jtsx-move-jsx-element-step-in-forward)
    (define-key mode-map (kbd "C-c C-S-<up>") 'jtsx-move-jsx-element-step-in-backward)
    (define-key mode-map (kbd "C-c j w") 'jtsx-wrap-in-jsx-element)
    (define-key mode-map (kbd "C-c j u") 'jtsx-unwrap-jsx)
    (define-key mode-map (kbd "C-c j d n") 'jtsx-delete-jsx-node)
    (define-key mode-map (kbd "C-c j d a") 'jtsx-delete-jsx-attribute)
    (define-key mode-map (kbd "C-c j t") 'jtsx-toggle-jsx-attributes-orientation)
    (define-key mode-map (kbd "C-c j h") 'jtsx-rearrange-jsx-attributes-horizontally)
    (define-key mode-map (kbd "C-c j v") 'jtsx-rearrange-jsx-attributes-vertically))
    
  (defun jtsx-bind-keys-to-jtsx-jsx-mode-map ()
      (jtsx-bind-keys-to-mode-map jtsx-jsx-mode-map))

  (defun jtsx-bind-keys-to-jtsx-tsx-mode-map ()
      (jtsx-bind-keys-to-mode-map jtsx-tsx-mode-map))

  (add-hook 'jtsx-jsx-mode-hook 'jtsx-bind-keys-to-jtsx-jsx-mode-map)
  (add-hook 'jtsx-tsx-mode-hook 'jtsx-bind-keys-to-jtsx-tsx-mode-map))

(use-package emmet-mode
  :ensure t
  :hook ((html-mode web-mode css-mode) . emmet-mode)
  :config
  (setq emmet-expand-jsx-className? t) ;; Optional for React/JSX
  (setq emmet-move-cursor-between-quotes t))

;;(with-eval-after-load 'emmet-mode
;;  (define-key emmet-mode-keymap (kbd "TAB") 'emmet-expand-line))


(use-package company-web
  :ensure t
  :after company
  :config
  (add-to-list 'company-backends 'company-web-html))

(use-package rainbow-delimiters)
(use-package smartparens)

(use-package diredfl
  :hook (dired-mode . diredfl-mode))

(when (and (boundp 'wakatime-api-key)
           wakatime-api-key)
  (use-package wakatime-mode
    :config
    (global-wakatime-mode 1)))

(use-package shackle)
(setq shackle-rules
      '((compilation-mode :align bottom :size 0.25 :select f)
        (grep-mode        :align bottom :size 0.25 :select f)
        (help-mode        :align right  :size 0.4 :select f)
        (Man-mode         :align right  :size 0.5 :select f)))
(shackle-mode 1)
;; (setq compilation-scroll-output 'first-error
;;       compilation-ask-about-save nil
;;       compilation-auto-jump-to-first-error t)

(use-package golden-ratio)

(defun my/split-window-right-and-focus ()
  "Split the window vertically and move focus to the new one."
  (interactive)
  (split-window-right)
  (other-window 1))

(defun my/split-window-below-and-focus ()
  "Split the window vertically and move focus to the new one."
  (interactive)
  (split-window-below)
  (other-window 1))

;; adds a frame in the middle where we use vertico
;; (use-package posframe
;;   :ensure t)

;; (use-package vertico-posframe
;;   :after vertico posframe
;;   :ensure t
;;   :custom
;;   (vertico-posframe-width 120)
;;   (vertico-posframe-min-width 50)
;;   (vertico-posframe-parameters '((left-fringe . 8) (right-fringe . 8)))
;;   :init
;;   (vertico-posframe-mode 1))

(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(use-package ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(use-package all-the-icons)
(use-package all-the-icons-ibuffer)

;;(use-package eglot
;;  :ensure t
;;  :hook (java-mode . eglot-ensure)
;;  :config
;;  ;; point to the Eclipse JDT Language Server if needed
;;  (add-to-list 'eglot-server-programs
;;               '(java-mode . ("jdtls"))))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

(use-package jetbrains-darcula-theme)
(use-package pyvenv)
(use-package fancy-compilation)

(use-package elfeed)
(setq elfeed-feeds
      '("http://nullprogram.com/feed/"
				"http://irreal.org/blog/"
				"https://archlinux.org/feeds/news/"
				"https://fosspost.org/feed"
				"https://feeds.feedburner.com/GoogleOpenSourceBlog"
        "https://planet.emacslife.com/atom.xml"))

(use-package gptel)
;; :key can be a function that returns the API key.
(defvar gemini-api-key nil
  "Gemini API key loaded from a secure untracked location.")
;; (gptel-make-gemini "gemini-2.5-pro" :key gemini-api-key :stream t)
;; (setq gptel-backend (gptel-get-backend "gemini-2.5-pro"))
;; (setq gptel-default-mode 'org-mode)
;; (setq gptel-default-major-mode 'org-mode)


(setq gptel-model 'qwen3:4b
      gptel-backend
      (gptel-make-ollama
       "Ollama"
       :host "localhost:11434"
       :stream t
       :models '(qwen3:4b)))

  (use-package claude-code)
  (global-set-key (kbd "C-c c a") 'claude-code-transient)

  (use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode)
  ;; If you have use-package-hook-name-suffix set to nil, uncomment and use the
  ;; line below instead:
  ;; :hook (eshell-mode-hook . esh-autosuggest-mode)
  :ensure t)

(use-package dumb-jump
 :ensure t
 :config
 (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
 (setq dumb-jump-force-searcher 'rg)
 ;; use completion-read instead of a separate buffer with candidates
 (setq xref-show-definitions-function #'xref-show-definitions-completing-read))

(use-package hl-todo
	:config (global-hl-todo-mode)
	)
(use-package consult-todo)

(use-package olivetti
	:config (setq olivetti-body-width 100)
	)
(use-package consult-todo)

  (setq mu4e-root-maildir (expand-file-name "~/Maildir/stensatter_mail"))
  (setq mu4e-sent-folder   "/Sent"
        mu4e-trash-folder  "/Trash"
        mu4e-drafts-folder "/Drafts")

  ;; Command to sync mail
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-update-interval 300)

  (setq user-mail-address "alfred@stensatter.se"
        user-full-name  "Alfred Roos")

  ;; SMTP configuration for sending mail
  (setq message-send-mail-function 'smtpmail-send-it
        smtpmail-smtp-server "send.one.com"
        smtpmail-smtp-service 587
  			smtpmail-smtp-user "alfred@stensatter.se"
        smtpmail-stream-type 'starttls)

  (add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
  (require 'mu4e)
  (use-package mu4e-alert)
  (use-package mu4e-alert)


  (add-hook 'message-mode-hook
  					(lambda ()
  						(flyspell-mode 1)
  						(my/middle)
  						))

(add-to-list 'mu4e-view-actions
  '("ViewInBrowser" . mu4e-action-view-in-browser) t)

(require 'llvm-mode)

;; Treemacs
;; (global-set-key [f8] 'treemacs)
;; (global-set-key (kbd "C-c o p") 'treemacs)
;; (global-set-key (kbd "C-c t t") 'treemacs-select-window)
;; (global-set-key (kbd "C-x p s") 'consult-ripgrep)


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

(defun my/compilation-open()
  (interactive)
  (switch-to-buffer (get-buffer "*compilation*"))
  )

(global-set-key (kbd "C-c c c") 'compile)
(global-set-key (kbd "C-c c C") 'recompile)
(global-set-key (kbd "C-c C-v") 'my/compilation-open)
(global-set-key (kbd "C-c v") 'my/compilation-open)
(global-set-key (kbd "C-c p c c") 'projectile-compile-project)

(global-set-key [f10] 'recompile)


(keymap-set compilation-mode-map "C-<return>" 'compilation-next-error)

(setq compilation-jump-to-first-error t)
(require 'ansi-color)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

(setq compilation-finish-functions
      (lambda (buf msg)
        (when (string-match ".*finished" msg)
          (when (featurep 'parrot) ;; if we have parrot we animate it 
            (parrot-start-animation))
          (message "Compilation finished successfully."))))

(setq display-buffer-alist
      '(("\\*compilation\\*"
         (display-buffer-reuse-window display-buffer-at-bottom)
         (window-height . 0.3)
         (inhibit-same-window . t))))

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

;;lsp
(global-set-key (kbd "C-c c d") 'lsp-goto-type-definition)

(keymap-set emacs-lisp-mode-map "C-c r" 'eval-buffer)

(use-package yasnippet)
(yas-global-mode 1)

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

;; (use-package mlscroll)
;; (use-package good-scroll)
;; (mlscroll-mode)
    ;; (good-scroll-mode)

(setq inhibit-startup-message t  ; Don't show the splash screen
      visible-bell t)            ; Flash when the bell rings
(setq inhibit-splash-screen t)
;; Disable unnecessary UI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq cursor-type 'bar)

(add-to-list 'default-frame-alist '(font . "Noto Sans Mono"))
(set-face-attribute 'default nil :font "Noto Sans Mono" :height 120)

;; Function to show current workspace in the mode-line
(defun my/mode-line-workspace ()
  "Return the current Eyebrowse workspace as a string for the mode-line."
  (let* ((current-slot (eyebrowse--get 'current-slot))
         (slots (eyebrowse--get 'window-configs))
         ;; Optional: mark active workspace
         (formatted-slots
          (mapcar (lambda (slot)
                    (let ((num (car slot)))
                      (if (= num current-slot)
                          (propertize (format "[%d]" num)
                                      'face '(:weight bold :foreground "green"))
                        (format "%d" num))))
                  slots)))
    ;; Join all slots into a string like "1 2 [3] 4"
    (string-join formatted-slots " ")))

(defun my/mode-line-scroll ()
  "Return the current scroll position in the buffer as a percentage for the mode-line."
  (let ((min (point-min))
        (max (point-max))
        (pos (point)))
    (if (= max min)
        "Top" ;; empty or very small buffer
      (format "%d%%" (/ (* 100 (- pos min)) (- max min))))))

(defun my/mode-line-wordcount ()
  "Return the word count of the buffer as a string for the mode-line.
Only displays for text-like modes (text, org, markdown)."
  (if (derived-mode-p 'text-mode 'org-mode 'markdown-mode)
      (format "%dW" (count-words (point-min) (point-max)))
    "")) ;; return empty string for other modes

(defun my/mode-line-git-branch ()
  "Return current Git branch for mode-line, or nil."
  (when-let ((file buffer-file-name)
             (backend (vc-backend file)))
    (when (eq backend 'Git)
      (when-let ((branch (vc-git--symbolic-ref file)))
        (concat " " branch)))))


(setq moody-mode-line-height 20)

(setq-default mode-line-format
  '(
    ;; Workspace (Eyebrowse)
    (:eval (my/mode-line-workspace))
    " | "
    
    ;; **Just use mode-line-buffer-identification**; Moody will style it automatically
    mode-line-buffer-identification
    " | Scroll: "
    
    ;; Scroll %
    (:eval (my/mode-line-scroll))
    " \% | "
    
    ;; Line number
    "Line: %l | Words: "
    
    ;; Word count
    (:eval (my/mode-line-wordcount))
    " | "
    
    ;; Space indicator
    (:eval (if indent-tabs-mode "Tab" "Spc"))
    " | "
    
    ;; Major mode
    mode-name
    " | "
    
    ;; Git branch
    (:eval (my/mode-line-git-branch))
    " | "
  
    ;; Time
    (:eval (format-time-string "%H:%M"))
		" | "
		(:eval (format-time-string "%A(%d) %B V%W "))
    ))

;; (require 'moody)
;; (moody-replace-mode-line-front-space)
;; (moody-replace-mode-line-buffer-identification)
;; (moody-replace-vc-mode)
;; (setq x-underline-at-descent-line t) ;; optional: underline style

;; Modern Org Mode Configuration
;; A sleek, feature-rich setup for Org mode

;; ============================================================================
;; CORE ORG SETTINGS
;; ============================================================================

(use-package org
  :ensure nil
  :init
    (unless (file-directory-p "~/org/")
    (make-directory "~/org/" t))
  :custom
  ;; Directories
  (org-directory "~/org/")
  (org-default-notes-file (concat org-directory "inbox.org"))
  
  ;; Visual settings
  (org-startup-indented t)
  (org-pretty-entities t)
  (org-hide-emphasis-markers t)
  (org-startup-with-inline-images t)
  (org-image-actual-width '(300))
  
  ;; Behavior
  (org-edit-src-content-indentation 0)
  (org-src-tab-acts-natively t)
  (org-src-preserve-indentation t)
  (org-return-follows-link t)
  (org-confirm-babel-evaluate nil))

(setq org-agenda-files
      (directory-files-recursively "~/org/" "\\.org$"))

(setq org-agenda-include-diary t)
;; Redisplay images after execution
(setq org-redisplay-inline-images t)
;; Or use a list for different sizes
(setq org-image-actual-width '(300 500 800))
(setq org-yank-image-save-method 'attach)
(setq org-yank-image-save-method "images/")

;; ============================================================================
;; MODERN BULLETS
;; ============================================================================

(use-package org-modern
  :ensure t
  :hook (org-mode . org-modern-mode)
  :custom
  (org-modern-star '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶"))
  (org-modern-table-vertical 1)
  (org-modern-table-horizontal 0.2)
  (org-modern-list '((43 . "➤") (45 . "–") (42 . "•")))
  (org-modern-todo-faces
   '(("TODO" :inverse-video t :inherit org-todo)
     ("PROG" :inverse-video t :inherit +org-todo-active)
     ("WAIT" :inverse-video t :inherit +org-todo-onhold)
     ("DONE" :inverse-video t :inherit org-done)))
  (org-modern-footnote (cons nil (cadr org-script-display)))
  (org-modern-block-fringe nil)
  (org-modern-block-name '("" . ""))
  (org-modern-keyword nil)
  (org-modern-timestamp t)
  (org-modern-priority t))

;; ============================================================================
;; ELEGANT FONTS
;; ============================================================================

;; (custom-set-faces
;;  '(org-level-1 ((t (:inherit outline-1 :height 1.4 :weight bold))))
;;  '(org-level-2 ((t (:inherit outline-2 :height 1.3 :weight semi-bold))))
;;  '(org-level-3 ((t (:inherit outline-3 :height 1.2 :weight semi-bold))))
;;  '(org-level-4 ((t (:inherit outline-4 :height 1.1 :weight semi-bold))))
;;  '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
;;  '(org-document-title ((t (:height 2.0 :weight bold :underline nil))))
;;  '(variable-pitch ((t (:family "ETBembo" :height 180 :weight normal))))
;;  '(fixed-pitch ((t (:family "JetBrains Mono" :height 140)))))

;; Fix mixed pitch for specific elements
;;(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
;;(set-face-attribute 'org-table nil :inherit 'fixed-pitch)
;;(set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
;;(set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
;;(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
;;(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
;;(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
;;(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

;; ============================================================================
;; BABEL LANGUAGES
;; ============================================================================

;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((emacs-lisp . t)
;;    (python . t)
;;    (shell . t)
;;    (js . t)
;;    (sql . t)))

;; ============================================================================
;; MODERN AGENDA VIEW
;; ============================================================================

;; (setq org-agenda-custom-commands
;;       '(("d" "Dashboard"
;;          ((agenda "" ((org-agenda-span 7)
;;                       (org-agenda-start-day "-1d")
;;                       (org-deadline-warning-days 7)))
;;           (todo "NEXT"
;;                 ((org-agenda-overriding-header "Next Actions")))
;;           (todo "PROG"
;;                 ((org-agenda-overriding-header "In Progress")))
;;           (todo "WAIT"
;;                 ((org-agenda-overriding-header "Waiting On")))))

;;         ("n" "Next Tasks"
;;          ((todo "NEXT"
;;                 ((org-agenda-overriding-header "Next Tasks")))))

;;         ("w" "Work"
;;          ((tags-todo "@work"
;;                      ((org-agenda-overriding-header "Work Tasks")))))))

;; ============================================================================
;; SUPER AGENDA (OPTIONAL - REQUIRES INSTALLATION)
;; ============================================================================

;; (use-package org-super-agenda
;;   :ensure t
;;   :after org-agenda
;;   :config
;;   (org-super-agenda-mode)
;;   :custom
;;   (org-super-agenda-groups
;;    '((:name "Today"
;;       :time-grid t
;;       :scheduled today)
;;      (:name "High Priority"
;;       :priority "A")
;;      (:name "In Progress"
;;       :todo "PROG")
;;      (:name "Next Actions"
;;       :todo "NEXT")
;;      (:name "Waiting"
;;       :todo "WAIT")
;;      (:name "Projects"
;;       :tag "project"))))

;; ============================================================================
;; EXPORT SETTINGS
;; ============================================================================

;; (setq org-export-with-smart-quotes t
;;       org-export-with-toc nil
;;       org-html-validation-link nil
;;       org-latex-listings 'minted
;;       org-latex-packages-alist '(("" "minted")))

;; ============================================================================
;; KEYBINDINGS
;; ============================================================================

;; (with-eval-after-load 'org
;;   (define-key org-mode-map (kbd "C-c C-x C-r") 'org-clock-report)
;;   (define-key org-mode-map (kbd "C-c C-x C-o") 'org-clock-out)
;;   (define-key org-mode-map (kbd "C-c C-x C-i") 'org-clock-in))

;; (provide 'org-config)

;; (setq treesit-language-source-alist
;;   '((c3 "https://github.com/c3lang/tree-sitter-c3")))
;; ;;(add-to-list 'treesit-language-source-alist
;; ;;  '(c3 "https://github.com/c3lang/tree-sitter-c3"))
;; (load "c3-ts-mode.el")
;; (require 'c3-ts-mode)

;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-server-programs '(c3-ts-mode "c3lsp")))

(load "odin-mode.el")

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)
(global-hl-line-mode 1)          ;; highlight current line

(setq-default indent-tabs-mode nil) ;; use spaces hehe
;;(setq-default tab-width 4)
;;(setq-default indent-tabs-mode t)
(setq-default tab-width 2) ; Assuming you want your tabs to be two spaces wide
(setq truncate-lines t)

(fset 'yes-or-no-p 'y-or-n-p)

(setq auto-save-default nil)
;; Optional: Change auto-save file location (to avoid clutter)
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.config/emacs/auto-saves/") t)))

(setq-default cursor-type 'box)  ; Slim vertical bar cursor
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

;; dired
(setq dired-listing-switches "-lah --group-directories-first")

(setq dashboard-banner-logo-title "There is no system but GNU, and Linux is one of its kernels")
(setq dashboard-center-content t)
;;(setq dashboard-startup-banner "~/Pictures/basta.png")
(setq dashboard-startup-banner "~/.config/emacs/banner.txt")
(setq dashboard-vertically-center-content t)
(dashboard-open)

(defun my/document ()
  (interactive)
  (dolist (theme custom-enabled-themes)
    (disable-theme theme))
  (load-theme 'doom-plain t)
  (display-line-numbers-mode 0)
  (olivetti-mode 1)
	(setq olivetti-body-width 110)
	(setq-local line-spacing 0.2)
  (set-face-attribute 'default nil
                      :font (font-spec
                             :family "Noto Serif Display"
                             :weight 'medium
                             :size 16)))

(defun jetbrains ()
  "Starts jetbrains style"
  (interactive)
  (load-theme 'jetbrains-darcula t)
  (projectile-switch-project)
  (treemacs))
(defun thanos/wtype-text (text)
  "Process TEXT for wtype, handling newlines properly."
  (let* ((has-final-newline (string-match-p "\n$" text))
         (lines (split-string text "\n"))
         (last-idx (1- (length lines))))
    (string-join
     (cl-loop for line in lines
              for i from 0
              collect (cond
                       ;; Last line without final newline
                       ((and (= i last-idx) (not has-final-newline))
                        (format "wtype -s 350 \"%s\"" 
                                (replace-regexp-in-string "\"" "\\\\\"" line)))
                       ;; Any other line
                       (t
                        (format "wtype -s 350 \"%s\" && wtype -k Return" 
                                (replace-regexp-in-string "\"" "\\\\\"" line)))))
     " && ")))

(defun edit ()
  "Open emcas config in a new buffer."
  (interactive)
  (find-file (concat (getenv "HOME") "/.config/emacs/readme.org")))

(defun my/system-update()
  (interactive)
  (async-shell-command "sudo pacman -Suyy --noconfirm"))

(defun my/middle()
	(interactive)
  (display-line-numbers-mode 0)
  (olivetti-mode 1)
	(setq olivetti-body-width 90))

(defun today()
	(interactive)
	(message (format-time-string "%A(%d) %B V%W ")))

(defun my/config()
  (interactive)
  (find-file "~/.config/emacs/readme.org"))


(defun erofi ()
  "Prompt using Vertico to run an executable from /usr/bin in a temporary frame."
  (interactive)
  ;; Set a temporary frame title
  (let ((frame (selected-frame))
        (files (seq-filter
                (lambda (f)
                  (file-executable-p (concat "/usr/bin/" f)))
                (directory-files "/usr/bin" nil "^[^.].*"))))
    (unwind-protect
        (progn
          (set-frame-parameter frame 'name "erofi") ;; set WM title
          ;; Prompt for executable
          (let ((choice (completing-read "Run: " files nil t)))
            (when (and choice (not (string= choice "")))
              ;; Run asynchronously
              (async-shell-command choice))))
       (delete-frame frame)
      )))

(defun my/open-file()
	(interactive)
	(let ((path (string-trim (thing-at-point 'string t) "\"" "\"")))
		(message "Opening file %s" path)
		(find-file path)))

(defun insert-uuid ()
  "Insert a new UUID at point."
  (interactive)
  (let ((uuid (if (fboundp 'uuidgen)
                  (uuidgen-4) ;; built-in uuidgen function in Emacs 28+
                (string-trim (shell-command-to-string "uuidgen")))))
    (insert uuid)))

(defun thanos/wtype-text (text)
  "Process TEXT for wtype, handling newlines properly."
  (let* ((has-final-newline (string-match-p "\n$" text))
         (lines (split-string text "\n"))
         (last-idx (1- (length lines))))
    (string-join
     (cl-loop for line in lines
              for i from 0
              collect (cond
                       ;; Last line without final newline
                       ((and (= i last-idx) (not has-final-newline))
                        (format "wtype -s 350 \"%s\"" 
                                (replace-regexp-in-string "\"" "\\\\\"" line)))
                       ;; Any other line
                       (t
                        (format "wtype -s 350 \"%s\" && wtype -k Return" 
                                (replace-regexp-in-string "\"" "\\\\\"" line)))))
     " && ")))

(defun thanos/type ()
  "Launch a temporary frame with a clean buffer for typing."
  (interactive)
  (let ((frame (make-frame '((name . "emacs-float")
                             (fullscreen . 0)
                             (undecorated . t)
                             (width . 70)
                             (height . 20))))
        (buf (get-buffer-create "emacs-float")))
    (select-frame frame)
    (switch-to-buffer buf)
    (erase-buffer)
    (org-mode)
    (setq-local header-line-format
                (format " %s to insert text or %s to cancel."
                        (propertize "C-c C-c" 'face 'help-key-binding)
			(propertize "C-c C-k" 'face 'help-key-binding)))
    (local-set-key (kbd "C-c C-k")
		   (lambda () (interactive)
		     (kill-new (buffer-string))
		     (delete-frame)))
    (local-set-key (kbd "C-c C-c")
		   (lambda () (interactive)
		     (start-process-shell-command
		      "wtype" nil
		      (thanos/wtype-text (buffer-string)))
		     (delete-frame)))))
