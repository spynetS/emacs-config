;; Prefer Tree-sitter major modes where available
(setq major-mode-remap-alist
      '((bash-mode . bash-ts-mode)
        (c-mode . c-ts-mode)
        (c++-mode . c++-ts-mode)
        (css-mode . css-ts-mode)
        (js-mode . js-ts-mode)
        (json-mode . json-ts-mode)
        (python-mode . python-ts-mode)
        (sh-mode . bash-ts-mode)
        (html-mode . html-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (yaml-mode . yaml-ts-mode)))

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (c "https://github.com/tree-sitter/tree-sitter-c")
        (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (css "https://github.com/tree-sitter/tree-sitter-css")
		(typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
		(tsx         . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
		(yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;; Install all grammars automatically if missing
(dolist (grammar treesit-language-source-alist)
  (unless (treesit-language-available-p (car grammar))
    (treesit-install-language-grammar (car grammar))))

