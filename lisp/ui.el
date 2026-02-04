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

(setq compilation-finish-functions
      (lambda (buf msg)
        (when (string-match ".*finished" msg)
          (message "Compilation finished successfully."))))

(setq display-buffer-alist
      '(("\\*compilation\\*"
         (display-buffer-reuse-window display-buffer-at-bottom)
         (window-height . 0.3)
         (inhibit-same-window . t))))

(setq inhibit-startup-message t  ; Don't show the splash screen
      visible-bell t)            ; Flash when the bell rings

;; Disable unnecessary UI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(mlscroll-mode)
(good-scroll-mode)

(add-to-list 'default-frame-alist '(font . "Iosevka-14"))
;;(add-to-list 'default-frame-alist '(font . "Iosevka" ))
(set-face-attribute 'default t :font "Iosevka" :weight 'bold )

(set-frame-font "Iosevka SemiBold-14" t t)

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

(require 'moody)
(moody-replace-mode-line-front-space)
(moody-replace-mode-line-buffer-identification)
(moody-replace-vc-mode)
(setq x-underline-at-descent-line t) ;; optional: underline style

;; Modern Org Mode Configuration
;; A sleek, feature-rich setup for Org mode

;; ============================================================================
;; CORE ORG SETTINGS
;; ============================================================================

(use-package org
  :ensure nil
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
  (org-confirm-babel-evaluate nil)
  
  ;; Todo keywords
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "PROG(p)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELLED(c@)")))
  
  (org-todo-keyword-faces
   '(("TODO" . (:foreground "#ff6c6b" :weight bold))
     ("NEXT" . (:foreground "#51afef" :weight bold))
     ("PROG" . (:foreground "#c678dd" :weight bold))
     ("WAIT" . (:foreground "#ecbe7b" :weight bold))
     ("DONE" . (:foreground "#98be65" :weight bold))
     ("CANCELLED" . (:foreground "#5b6268" :weight bold))))

(org-agenda-files (list org-directory))
  (org-agenda-start-with-log-mode t)
  (org-log-done 'time)
  (org-log-into-drawer t)
  
  ;; Tags
  (org-tag-alist
   '((:startgroup)
     ("@work" . ?w)
     ("@home" . ?h)
     ("@errands" . ?e)
     (:endgroup)
     ("planning" . ?p)
     ("review" . ?r)
     ("note" . ?n)))
  
  ;; Capture templates
  (org-capture-templates
   '(("t" "Todo" entry (file+headline org-default-notes-file "Tasks")
      "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)
     ("m" "Meeting" entry (file+headline org-default-notes-file "Meetings")
      "* %? :meeting:\n  %U" :empty-lines 1)
     ("n" "Note" entry (file+headline org-default-notes-file "Notes")
      "* %?\n  %U\n  %i" :empty-lines 1)
     ("j" "Journal" entry (file+datetree "~/org/journal.org")
      "* %?\n  Entered on %U\n  %i" :empty-lines 1)))
  
  :bind
  (("C-c o l" . org-store-link)
   ("C-c o a" . org-agenda)
   ("C-c o c" . org-capture)
   :map org-mode-map
   ("C-c C-q" . counsel-org-tag))
  
  :hook
  (org-mode . visual-line-mode)
  (org-mode . variable-pitch-mode))

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

(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :height 1.4 :weight bold))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.3 :weight semi-bold))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.2 :weight semi-bold))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.1 :weight semi-bold))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
 '(org-document-title ((t (:height 2.0 :weight bold :underline nil))))
 '(variable-pitch ((t (:family "ETBembo" :height 180 :weight normal))))
 '(fixed-pitch ((t (:family "JetBrains Mono" :height 140)))))

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

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (shell . t)
   (js . t)
   (sql . t)))

;; ============================================================================
;; MODERN AGENDA VIEW
;; ============================================================================

(setq org-agenda-custom-commands
      '(("d" "Dashboard"
         ((agenda "" ((org-agenda-span 7)
                      (org-agenda-start-day "-1d")
                      (org-deadline-warning-days 7)))
          (todo "NEXT"
                ((org-agenda-overriding-header "Next Actions")))
          (todo "PROG"
                ((org-agenda-overriding-header "In Progress")))
          (todo "WAIT"
                ((org-agenda-overriding-header "Waiting On")))))
        
        ("n" "Next Tasks"
         ((todo "NEXT"
                ((org-agenda-overriding-header "Next Tasks")))))
        
        ("w" "Work"
         ((tags-todo "@work"
                     ((org-agenda-overriding-header "Work Tasks")))))))

;; ============================================================================
;; SUPER AGENDA (OPTIONAL - REQUIRES INSTALLATION)
;; ============================================================================

(use-package org-super-agenda
  :ensure t
  :after org-agenda
  :config
  (org-super-agenda-mode)
  :custom
  (org-super-agenda-groups
   '((:name "Today"
      :time-grid t
      :scheduled today)
     (:name "High Priority"
      :priority "A")
     (:name "In Progress"
      :todo "PROG")
     (:name "Next Actions"
      :todo "NEXT")
     (:name "Waiting"
      :todo "WAIT")
     (:name "Projects"
      :tag "project"))))

;; ============================================================================
;; EXPORT SETTINGS
;; ============================================================================

(setq org-export-with-smart-quotes t
      org-export-with-toc nil
      org-html-validation-link nil
      org-latex-listings 'minted
      org-latex-packages-alist '(("" "minted")))

;; ============================================================================
;; KEYBINDINGS
;; ============================================================================

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c C-x C-r") 'org-clock-report)
  (define-key org-mode-map (kbd "C-c C-x C-o") 'org-clock-out)
  (define-key org-mode-map (kbd "C-c C-x C-i") 'org-clock-in))

(provide 'org-config)

(setq display-line-numbers 'relative)
(global-display-line-numbers-mode 1)
(global-hl-line-mode +1)          ;; highlight current line

;;(setq-default indent-tabs-mode nil) ;; use spaces hehe
;;(setq-default tab-width 4)
(setq-default indent-tabs-mode t)
(setq-default tab-width 2) ; Assuming you want your tabs to be two spaces wide
(setq truncate-lines t)

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
