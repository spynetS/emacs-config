;; Load custom Emacs Lisp files
(add-to-list 'load-path "/home/spy/.config/emacs/lisp")
(setq browse-url-browser-function 'browse-url-firefox)

;; Java Configuration
(setenv "JAVA_HOME" "/usr/lib/jvm/java-25-openjdk")
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

;; Load configuration modules
(load "packages")
(load "ui")
(load "keybindings")
(load "treesitter")
(load "secret")

;; Custom file configuration
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

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

(use-package emms
  :config
  (emms-all)
  (emms-default-players)
  (emms-history-load))

(use-package emms-player-spotify
  :straight (emms-player-spotify :type git :host github :repo "sarg/emms-spotify")

  :custom
  (emms-player-spotify-launch-cmd "flatpak run com.spotify.Client")
  (emms-player-spotify-adblock t)

  :config
  (add-to-list 'emms-player-list emms-player-spotify))

(use-package exwm
  :ensure t
  :config
  ;; Number of workspaces
  (setq exwm-workspace-number 4)

  ;; Make class name the buffer name
  (add-hook 'exwm-update-class-hook
            (lambda ()
              (exwm-workspace-rename-buffer exwm-class-name)))

  ;; Global keybindings (these work everywhere)
  (setq exwm-input-global-keys
        `(
          ;; Reset to line-mode (C-c C-k)
          ([?\s-r] . exwm-reset)
          ;; Switch workspace
          ([?\s-w] . exwm-workspace-switch)
          ;; Launch application
          ([?\s-&] . (lambda (command)
    
                   (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))))
(defun my-exwm-terminal ()
  (interactive)
  (start-process "terminal" nil "kitty"))

(exwm-input-set-key (kbd "s-<return>") 'my-exwm-terminal)
	
  ;; Line-mode vs char-mode switching
  (define-key exwm-mode-map [?\C-q] #'exwm-input-send-next-key)

;; Optional: switch workspaces with Super+number
(dotimes (i exwm-workspace-number)
  (exwm-input-set-key (kbd (format "s-%d" (1+ i)))
                      `(lambda () (interactive) (exwm-workspace-switch ,i))))

;; show some info
(display-battery-mode)
(display-time)
(exwm-modeline-mode)


;; Enable system tray in EXWM
(require 'exwm-systemtray)
(exwm-systemtray-enable)

;; Optional: configure refresh interval (in seconds)
(setq exwm-systemtray-refresh-interval 2)

  ;; Enable EXWM
  (exwm-enable))
