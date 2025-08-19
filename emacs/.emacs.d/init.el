(add-to-list 'load-path "~/.emacs.d/config")

;; (add-to-list 'default-frame-alist
;;              '(font . "Inconsolata-12"))

(load "packages.el")
(load "org-config.el")
(load "org-roam-config.el")
(load "keybindings.el")

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(global-hl-line-mode)

(electric-pair-mode t)
(show-paren-mode 1)
(setq-default indent-tabs-mode nil)
(save-place-mode t)
(savehist-mode t)
(recentf-mode t)
(global-auto-revert-mode t)

(defadvice load-theme (before theme-dont-propagate activate)
  (mapc #'disable-theme custom-enabled-themes))

(setq harwiltz/dark-theme 'kaolin-ocean)
(setq harwiltz/light-theme 'modus-operandi)
(setq harwiltz/apply-dark-theme nil)

(defun harwiltz/load-theme ()
  (interactive)
  (if harwiltz/apply-dark-theme (load-theme harwiltz/dark-theme) (load-theme harwiltz/light-theme)))

(defun harwiltz/toggle-theme ()
  (interactive)
  (setq harwiltz/apply-dark-theme (not harwiltz/apply-dark-theme))
  (harwiltz/load-theme))

(harwiltz/load-theme)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward
      window-resize-pixelwise t
      frame-resize-pixelwise t
      load-prever-newer t
      backup-by-copying t
      custom-file (expand-file-name "custom.el" user-emacs-directory))
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(load custom-file t)

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(unless package-archive-contents
  (package-refresh-contents))
