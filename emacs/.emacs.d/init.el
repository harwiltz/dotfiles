(set-face-attribute 'default nil :height 120 :family "Ubuntu Mono") ; Remove later

(add-to-list 'load-path "~/.emacs.d/config")
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(setq custom-file "~/.emacs.d/emacs-custom.el")

(setq harwiltz/agenda-dir "~/agenda")

(load "packages.el")
(load "lsp.el")
(load "org-config.el")
(load "org-roam-config.el")
(load "keybindings.el")
(load "handy-functions.el")
(and (file-exists-p harwiltz/agenda-dir) (load "agenda.el"))

(load custom-file t)

(defadvice load-theme (before theme-dont-propagate activate)
  (mapc #'disable-theme custom-enabled-themes))

;; Nice themes:
;; - ample (dark)
;; - modus-operandi-tinted (light)
;; - modus-vivendi (dark)
;; - gruvbox (dark)
;; - doom-gruvbox (dark)
;; - kaolin-breeze (light)
;; - kaolin-shiva (dark)
;; - spacemacs-dark (dark)
;; - spacemacs-light (light)

(setq harwiltz/dark-theme 'spacemacs-dark)
(setq harwiltz/light-theme 'spacemacs-light)
(setq harwiltz/apply-dark-theme t)

(defun harwiltz/load-theme ()
  (interactive)
  (if harwiltz/apply-dark-theme (load-theme harwiltz/dark-theme) (load-theme harwiltz/light-theme)))

(defun harwiltz/toggle-theme ()
  (interactive)
  (setq harwiltz/apply-dark-theme (not harwiltz/apply-dark-theme))
  (harwiltz/load-theme))

(harwiltz/load-theme)
;; (setq catppuccin-flavor 'mocha)
;; (load-theme 'catppuccin)

(blink-cursor-mode 0)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(global-hl-line-mode)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(custom-theme-set-faces
 'user
 '(variable-pitch ((t (:family "Linux Libertine" :height 120 :weight thin))))
 '(fixed-pitch ((t (:family "Ubuntu Mono" :height 120)))))
