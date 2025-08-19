(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/zettelkasten")
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (org-roam-db-autosync-mode))

(setq harwiltz/org-roam-filename-template "%<%Y%m%d%H%M%S>-${slug}.org")
(setq harwiltz/org-roam-skunkworks-dir "skunkworks")

(setq org-roam-capture-templates
      `(
        ("d" "default" plain "%?"
         :target (file+head ,harwiltz/org-roam-filename-template "#+title: ${title}"))
        ("s" "skunkworks" plain "%?"
         :target (file+head
                  ,(concat harwiltz/org-roam-skunkworks-dir "/" harwiltz/org-roam-filename-template)
                  "#+title: ${title}")
         :unnarrowed t)))
