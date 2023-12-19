(set-face-attribute 'default nil :height 120 :family "Inconsolata") ; Remove later

(add-to-list 'load-path "~/.emacs.d/config")
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(setq custom-file "~/.emacs.d/emacs-custom.el")

(setq harwiltz/agenda-dir "~/agenda")

(load "packages.el")
(load "lsp.el")
(load "org-config.el")
(load "org-roam-config.el")
(load "keybindings.el")
(and (file-exists-p harwiltz/agenda-dir) (load "agenda.el"))
(load custom-file t)
;; (load-theme 'modus-operandi)
(setq catppuccin-flavor 'mocha)
(load-theme 'catppuccin)

(blink-cursor-mode 0)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(global-hl-line-mode)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(custom-theme-set-faces
 'user
 '(variable-pitch ((t (:family "Linux Libertine" :height 120 :weight thin))))
 '(fixed-pitch ((t (:family "Inconsolata" :height 120)))))
