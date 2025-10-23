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


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor-in-non-selected-windows nil)
 '(custom-safe-themes
	 '("0325a6b5eea7e5febae709dab35ec8648908af12cf2d2b569bedc8da0a3a81c1"
		 "9b55271bec4b2bae7eca6c96eac974b19f2f6d8cccc8fd34b30ab67220bb19d5"
		 "720838034f1dd3b3da66f6bd4d053ee67c93a747b219d1c546c41c4e425daf93"
		 "5c7720c63b729140ed88cf35413f36c728ab7c70f8cd8422d9ee1cedeb618de5"
		 "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8"
		 default))
 '(package-selected-packages nil)
 '(wakatime-cli-path "/usr/bin/wakatime-cli"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
