;; Load custom Emacs Lisp files
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
(load "packages")
(load "ui")
(load "keybindings")
(load "treesitter")


;; Custom file configuration
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(setq compilation-jump-to-first-error nil)
(require 'ansi-color)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

  (load "c3-mode.el")

(defun today()
	(interactive)
	(message (format-time-string "%A(%d) %B V%W ")))

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
