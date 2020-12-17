(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; basic emacs properties
(blink-cursor-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)
(global-display-line-numbers-mode 1)
(display-time)
(setq doc-view-continuous t)

(setq load-path (cons "~/.emacs.d/lisp" load-path))
(require 'assemble)

;; hotkeys
(global-set-key (kbd "M-e") 'eshell)
(global-set-key (kbd "M-s") 'multi-term)

;; org-mode stuff
(eval-after-load 'org '(add-to-list 'org-file-apps '("\\.pdf\\'" . emacs)))
(setq org-latex-create-formula-image-program 'dvipng)
(setq org-todo-keywords '((sequence "TODO" "IN PROGRESS" "|" "DONE" "PASS")))
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t) (haskell . t)))

(setq org-agenda-files
      (let ((base "~/research"))
	`(,base
    ,(concat base "/backlog")
	  ,(concat base "/notes")
	  ,(concat base "/papers"))))

;; shell stuff
(setq explicit-shell-file-name "/bin/bash")
(setq multi-term-program "/bin/bash")

(defun insderiv ()
  "Insert equation* aligned"
  (interactive)
  (insert "\\begin{equation*}\n")
  (insert "\\begin{aligned}\n")
  (insert "\\end{aligned}\n")
  (insert "\\end{equation*}")
  (forward-line -2)
  (end-of-line))

(defun inseq (label)
  "Insert labeled equation"
  (interactive "sEnter label: ")
  (insert (format "\\begin{equation}\\label{eq:%s}\n" label))
  (insert "\\end{equation}")
  (forward-line -1)
  (end-of-line))

(defun insthm (type header)
  "Insert text block with header"
    (interactive "sBlock type: \nsHeader: ")
    (insert "#+ATTR_LATEX: :options [" header "]\n")
    (insert "#+NAME: " header "\n")
    (insert "#+begin_" (downcase type) "\n")
    (insert ":PROPERTIES:\n")
    (insert ":CUSTOM_ID: " (idify header) "\n")
    (insert ":END:\n")
    (insert "#+end_" (downcase type) "\n")
    (forward-line -2)
    (end-of-line))

(defun idify (s)
  "Make id string from arbitrary string"
  (downcase (string-join (split-string s " ") "-")))

(defun cid (s)
  (interactive "sHeader: ")
  (insert ":PROPERTIES:\n:CUSTOM_ID: " (idify s) "\n:END:\n"))

(defun wc ()
  "Count words in buffer"
  (interactive)
  (save-excursion
    (save-restriction
      (if (use-region-p)
	  (narrow-to-region region-beginning region end)
	(widen))
      (goto-char (if (boundp 'point-min) point-min 0))
      (let ((ws (count-matches "\\sw+")))
	(message "Words: %d" ws)))))

(require 'ox-publish)
(defun org-jekyll-add-project (path)
  (interactive "sProject path: ")
  (let* ((bdir (concat path "/org/posts"))
	(pdir (concat path "/_posts"))
	(adir (concat path "/assets"))
	(notes `("org-notes"
		 :base-directory ,(identity bdir)
		 :base-extension "org"
		 :publishing-directory ,(identity pdir)
		 :recursive t
		 :publishing-function org-html-publish-to-html
		 :body-only t
		 :auto-preamble t
		 :html-extension "html"
		))
	(static `("org-static"
		  :base-directory ,(identity bdir)
		  :base-extension "css\\|js\\|png\\|jpg\\|gif\\|webp\\|pdf\\|"
		  :publishing-directory ,(identity adir)
		  :publishing-function org-publish-attachment
		 ))
	(publish '("org" :components ("org-notes" "org-static"))))
    (setq org-publish-project-alist
	  (if (boundp 'org-publish-project-alist)
	      (nconc (list notes static publish) (car org-publish-project-alist))
	    (list notes static publish)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:
;; Let emacs do its thing after the following line: ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#0a0814" "#f2241f" "#67b11d" "#b1951d" "#4f97d7" "#a31db1" "#28def0" "#b2b2b2"])
 '(custom-enabled-themes '(ample))
 '(custom-safe-themes
   '("f126f3b6ca7172a4a2c44186d57e86a989c2c196d0855db816a161bf857b58fb" "36ca8f60565af20ef4f30783aa16a26d96c02df7b4e54e9900a5138fb33808da" "c9ddf33b383e74dac7690255dd2c3dfa1961a8e8a1d20e401c6572febef61045" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default))
 '(fci-rule-color "#14151E")
 '(hl-todo-keyword-faces
   '(("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#4f97d7")
     ("OKAY" . "#4f97d7")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#86dc2f")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX+" . "#dc752f")
     ("\\?\\?\\?+" . "#dc752f")))
 '(package-selected-packages
   '(afternoon-theme inkpot-theme htmlize ample-theme haskell-mode multi-term spacemacs-theme evil))
 '(pdf-view-midnight-colors '("#b2b2b2" . "#292b2e"))
 '(send-mail-function 'smtpmail-send-it)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#d54e53")
     (40 . "goldenrod")
     (60 . "#e7c547")
     (80 . "DarkOliveGreen3")
     (100 . "#70c0b1")
     (120 . "DeepSkyBlue1")
     (140 . "#c397d8")
     (160 . "#d54e53")
     (180 . "goldenrod")
     (200 . "#e7c547")
     (220 . "DarkOliveGreen3")
     (240 . "#70c0b1")
     (260 . "DeepSkyBlue1")
     (280 . "#c397d8")
     (300 . "#d54e53")
     (320 . "goldenrod")
     (340 . "#e7c547")
     (360 . "DarkOliveGreen3")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'emacs)
(evil-mode 1)

(defun org-export-pdf-then-open()
  (org-open-file (org-latex-export-to-pdf)))
(defun org-auto-export-on() "auto export to pdf when saving an org file" (interactive)
       (when (eq major-mode 'org-mode) (add-hook 'after-save-hook 'org-export-pdf-then-open t t) ) )
