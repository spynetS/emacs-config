(add-to-list 'load-path "/home/spy/.config/emacs/lisp")

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
;; for lsp
(setq gc-cons-threshold 100000000) ;; 100 MB

(load "packages")
(load "ui")
(load "keybindings")
(load "treesitter")
(load "secret")

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
