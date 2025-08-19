(use-package websocket :ensure t)
(use-package typst-preview
  :load-path "typst-preview.el"
  :config
  (setq typst-preview-browser "default")
  (define-key typst-preview-mode-map (kbd "C-c C-j") 'typst-preview-send-position))

(add-to-list 'treesit-language-source-alist
             '(typst "https://github.com/uben0/tree-sitter-typst"))
(treesit-install-language-grammar 'typst)
