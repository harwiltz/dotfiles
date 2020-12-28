;; base16-brewer-theme.el -- A base16 colorscheme

;;; Commentary:
;; Base16: (https://github.com/chriskempson/base16)

;;; Authors:
;; Scheme: Timothée Poisot (http://github.com/tpoisot)
;; Template: Kaleb Elwert <belak@coded.io>

;;; Code:

(require 'base16-theme)

(setq wiltz-base16-brewer-colors
  '(:base00 "#0c0d0e"   ;; background
    :base01 "#2e2f30"   ;; line number background
    :base02 "#2c2c2c"   ;; status background, comment symbol?
    :base03 "#737475"   ;; comment foreground
    :base04 "#959697"   ;; status foreground
    :base05 "#b7b8b9"   ;; some foreground stuff
    :base06 "#dadbdc"   ;; no idea
    :base07 "#fcfdfe"   ;; no idea
    :base08 "#999900"   ;; cursor
    :base09 "#e6550d"   ;; local variable keyword
    :base0A "#dca060"   ;; macro name?
    :base0B "#31a354"   ;; status bar file path
    :base0C "#80b1d3"   ;; the :base things
    :base0D "#3182bd"   ;; no idea
    :base0E "#756bb1"   ;; setq, require, deftheme, defun...
    :base0F "#b15928")) ;; no idea
 ;; "All colors for Base16 Brewer are defined here.")

;; Define the theme
(deftheme wiltz-base16-brewer)

;; Add all the faces to the theme
(base16-theme-define 'wiltz-base16-brewer wiltz-base16-brewer-colors)

;; Mark the theme as provided
(provide-theme 'wiltz-base16-brewer)

(provide 'wiltz-base16-brewer-theme)

;;; base16-brewer-theme.el ends here
