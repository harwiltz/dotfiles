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
  (setq bibtex-completion-bibliography "~/zotero-sources.bib"
	bibtex-completion-library-path "~/zettelkasten/library"
	bibtex-completion-notes-path "~/zettelkasten/paper-notes"
	bibtex-completion-pdf-field "file"
	bibtex-completion-notes-template-multiple-files
	"#+TITLE: [Notes] ${title}
#+AUTHOR: ${author-or-editor}

#+BEGIN_SRC bibtex
@${=type=}{
    title={${title}},
    author={${author}},
    year={${year}}
}
#+END_SRC

"
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

(use-package spacemacs-theme :ensure t)

(use-package yasnippet
  :ensure t
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets")))

(use-package magit :ensure t)

(use-package markdown-mode :ensure t)

(use-package dimmer
  :ensure t
  :custom (dimmer-fraction 0.3)
  :config (dimmer-mode))
