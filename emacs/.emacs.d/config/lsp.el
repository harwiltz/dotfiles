(use-package elglot
  :bind (:map eglot-mode-map
	      ("C-c d" . eldoc)
	      ("C-c a" . eglot-code-actions)
	      ("C-c r" . eglot-rename)))

(use-package rust-ts-mode
  :ensure t
  :mode ("\\.rs\\'" . rust-ts-mode)
  :hook ((rust-ts-mode . eglot-ensure)
	 (rust-ts-mode . company-mode))
  :config
  (setenv "PATH" (concat (getenv "PATH") ":/usr/lib/rustup/bin"))
  (add-to-list 'exec-path "/usr/lib/rustup/bin"))

(use-package python-ts-mode
  :hook ((python-ts-mode . eglot-ensure)
	 (python-ts-mode . company-mode))
  :mode ("\\.py\\'" . python-ts-mode))

(use-package pipenv
  :ensure t
  :hook ((python-mode . pipenv-mode)))

(use-package pyvenv :ensure t)
