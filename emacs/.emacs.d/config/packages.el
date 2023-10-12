(use-package evil
  :ensure t
  :init
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1))

(use-package vertico
  :ensure t
  :config
  (vertico-mode t))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode t))

(use-package auctex
  :ensure t
  :defer t
  :hook
  ((LaTeX-mode . (lambda () (add-to-list 'TeX-view-program-selection
					 '(output-pdf  "Zathura"))))
   (LaTeX-mode . auto-fill-mode)))

(use-package bibtex-completion
  :ensure t
  :config
  (setq bibtex-completion-bibliography "~/zettelkasten/sources.bib"
	bibtex-completion-library-path "~/zettelkasten/library"
	bibtex-completion-pdf-open-function
	(lambda (path) (call-process "zathura" nil 0 nil path))))

(use-package helm-bibtex :ensure t)

(use-package company
  :ensure t)

(use-package org-bullets
  :ensure t
  :hook (org-mode . (lambda () (org-bullets-mode 1))))

(use-package org-ref :ensure t)

(use-package catppuccin-theme :ensure t)
