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

(use-package treemacs)
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
  :hook ((java-mode . lsp))
  :commands lsp
  :config
  (setq lsp-prefer-flymake nil   ;; Use flycheck instead of flymake
        lsp-enable-snippet t
        lsp-completion-provider :capf))

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-code-actions t))

(use-package lsp-treemacs
  :after lsp)

(use-package company
  :hook ((java-mode . company-mode))
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0))

(use-package dap-mode
  :after lsp-mode
  :config
  (dap-auto-configure-mode)
  ;; Enable java debugging support
  (require 'dap-java))

;; LSP Java
(use-package lsp-java
  :after lsp
  :config
  (add-hook 'java-mode-hook #'lsp)
  ;; Optional: auto import
  (setq lsp-java-format-settings-url "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml"
        lsp-java-format-settings-profile "GoogleStyle"))

(use-package all-the-icons :if (display-graphic-p))

(use-package doom-themes
  :init
  (load-theme 'doom-one t))

(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  :custom
  ;; Optional tweaks
  (doom-modeline-height 25)
  (doom-modeline-bar-width 3)
  (doom-modeline-buffer-file-name-style 'truncate-with-project)
  (doom-modeline-minor-modes nil)
  (doom-modeline-indent-info t)
  (doom-modeline-icon t))  ;; requires all-the-icons package

(use-package multiple-cursors)

(use-package eyebrowse)
(eyebrowse-mode)

(use-package jtsx
  :ensure t
  :mode (("\\.jsx?\\'" . jtsx-jsx-mode)
         ("\\.tsx\\'" . jtsx-tsx-mode)
         ("\\.ts\\'" . jtsx-typescript-mode))
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

(use-package rainbow-delimiters)
(use-package smartparens)
(use-package kdl-mode)

(use-package diredfl
  :hook (dired-mode . diredfl-mode))

(use-package wakatime-mode)
(global-wakatime-mode 1)

(use-package shackle)
(setq shackle-rules
      '((compilation-mode :align bottom :size 0.25 :select f)
        (grep-mode        :align bottom :size 0.25 :select f)
        (help-mode        :align right  :size 0.4 :select f)
        (Man-mode         :align right  :size 0.5 :select f)))
(shackle-mode 1)
(setq compilation-scroll-output 'first-error
      compilation-ask-about-save nil
      compilation-auto-jump-to-first-error t)

;; adds a frame in the middle where we use vertico
(use-package posframe
  :ensure t)

(use-package vertico-posframe
  :after vertico posframe
  :ensure t
  :custom
  (vertico-posframe-width 120)
  (vertico-posframe-min-width 50)
  (vertico-posframe-parameters '((left-fringe . 8) (right-fringe . 8)))
  :init
  (vertico-posframe-mode 1))

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

(use-package mlscroll)
(use-package good-scroll)
