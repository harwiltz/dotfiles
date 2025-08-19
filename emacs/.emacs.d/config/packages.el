(use-package evil
  :ensure t
  :init
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1))

(use-package vertico
  :ensure t
  :config
  (vertico-mode t)
  :bind (:map vertico-map
              ("M-DEL" . vertico-directory-delete-word)))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode t))

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package citar
  :ensure t
  :custom
  (citar-bibliography '("~/zotero-sources.bib")))

(use-package company :ensure t)

(use-package avy  ; fast navigation
  :ensure t
  :config
  (global-set-key (kbd "C-;") 'avy-goto-char)
  (setq avy-all-windows 'all-frames))

(use-package eat :ensure t)

(use-package powerthesaurus :ensure t)

(use-package org-bullets
  :ensure t
  :hook (org-mode . (lambda () (org-bullets-mode 1))))

(use-package org-ref :ensure t)

(use-package olivetti  ; margins in org-mode
  :ensure t)

(use-package catppuccin-theme :ensure t)

(use-package kaolin-themes :ensure t)

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

(use-package yasnippet
  :ensure t
  :hook ((text-mode prog-mode conf-mode snippet-mode) . yas-minor-mode-on)
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets")))

(use-package magit :ensure t)
