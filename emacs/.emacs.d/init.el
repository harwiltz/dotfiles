(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; basic emacs properties
(blink-cursor-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)
(display-time)
(setq x-select-enable-clipboard t)
(setq org-roam-v2-ack t)
(setq custom-safe-themes t)
(setq evil-want-C-i-jump nil)

;; color themes
(setq harwiltz/light-theme 'base16-atelier-dune-light)
(setq harwiltz/dark-theme 'doom-sourcerer)
(setq harwiltz/pdoc-process nil)

(setq harwiltz/font "UbuntuMono-12")

;; deft stuff
(setq deft-extensions '("org" "tex"))
(setq deft-directory "/home/harwiltz/zettelkasten")
(setq deft-recursive t)
(setq deft-use-filter-string-for-filename t)
(setq deft-use-filename-as-title t)
(global-set-key (kbd "C-c o") 'deft)

(defun reset-font (&optional font size)
  (interactive "sFont family: \nsFont size: ")
  (let ((f (or font harwiltz/font))
	(s (or size "12")))
    (set-face-attribute 'default nil :font (concat f "-" s))))

(setq harwiltz/use-dark-theme t)

(defun install-missing-packages ()
  (interactive)
  (let ((packages-exist
	 (seq-reduce (lambda (a b) (and a b))
		     (mapcar 'package-installed-p package-selected-packages)
		     t)))
    (when packages-exist
      (message "All packages are installed."))
    (unless packages-exist
      (message "Refreshing package metadata...")
      (package-refresh-contents)
      (package-install-selected-packages)
      (message "Finished installing missing packages."))))

;; Tab bar stuff
(set-face-attribute
 'tab-bar nil
 :family "Terminus"
 :background "gray20"
 :foreground "gray80"
 :box nil
 :height 0.75)
(set-face-attribute
 'tab-bar-tab nil
 :background "gray20"
 :foreground "yellow"
 :box nil
 :height 1.0)
(set-face-attribute
 'tab-bar-tab-inactive nil
 :background "gray20"
 :foreground "gray80"
 :box nil
 :height 1.0)

(defun harwiltz/toggle-theme ()
  (interactive)
  (setq harwiltz/use-dark-theme (not harwiltz/use-dark-theme))
  (let ((new (if harwiltz/use-dark-theme harwiltz/dark-theme harwiltz/light-theme))
	(old (if harwiltz/use-dark-theme harwiltz/light-theme harwiltz/dark-theme)))
    (cons (disable-theme old) (enable-theme new))))

(global-set-key (kbd "<f6>") 'harwiltz/toggle-theme)

;; regenerate scratch buffer
(defun harwiltz/scratch (&optional name)
  (interactive "sEnter name for buffer (default 'scratch'): ")
  (let ((buf (if (string-empty-p (or name ""))
		 "scratch"
	       name)))
    (switch-to-buffer (format "*%s*" buf))))

(defun harwiltz/killall (regex)
  (interactive "sEnter regex: ")
  (kill-matching-buffers regex nil t))

(defvar pdoc-server-buffer-name "pdoc server")

(defun run-pdoc-server (dir &optional port)
  (interactive "sEnter server root: \nsPort (8080): ")
  (setenv "PYTHONPATH" (concat (pwd) ":" (getenv "PYTHONPATH")))
  (let ((root (or dir (pwd)))
	(port (if (and port (not (string-empty-p port)))
		  (string-to-number port)
		8080)))
    (shell-command (concat "pdoc3 --html --http localhost:" (number-to-string port)
			   " -c latex_math=True " root "&")
		   (format "*%s %s*" pdoc-server-buffer-name (file-name-base root)))))
			 
(defun kill-pdoc-server ()
  (interactive)
  (if harwiltz/pdoc-process
      (cons (delete-process harwiltz/pdoc-process)
	    (setq harwiltz/pdoc-process nil))
    (message "There is no pdoc process running")))

(require 'doc-view)
(setq doc-view-continuous t)

(setq load-path (cons "~/.emacs.d/lisp" load-path))
(setq load-path (cons "~/.emacs.d/lisp/assemb.el" load-path))
(setq load-path (cons "~/.emacs.d/lisp/hatch-button" load-path))
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; configuring my scripts
(setq parcel-split-window 0) ;; split horizontally
(setq parcel-pagerank-runs 10)

;; highlight line
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
;; (add-hook 'hl-line-mode-hook (lambda () (set-face-background 'hl-line "#181818")))

;; hotkeys
(global-set-key (kbd "M-e") 'eshell)
(global-set-key (kbd "M-s") 'harwiltz/scratch)
(global-set-key (kbd "C-c p") 'parcel-add-zettel)
(global-set-key (kbd "C-c b") 'parcel-add-reference)
(global-set-key (kbd "C-c P") 'parcel-assemble-all)
(global-set-key (kbd "C-c v") (lambda () (interactive) (assemble nil)))
(define-key doc-view-mode-map (kbd "j") 'doc-view-next-line-or-next-page)
(define-key doc-view-mode-map (kbd "k") 'doc-view-previous-line-or-previous-page)

;; latex/auctex stuff
(defun harwiltz/init-latex ()
  (message "Initializing latex stuff...")
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq org-latex-inputenc-alist '(("utf8" . "utf8x")))
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook 'TeX-fold-mode)
  (add-hook 'LaTeX-mode-hook 'auto-fill-mode)
  (setq reftex-plug-into-AUCTeX t)
  (setq reftex-ref-style-default-list (list "Hyperref" "Default")))

(defun harwiltz/tex-fold-envs ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\\\begin{" nil t)
      (TeX-fold-env))))

(setq bibtex-completion-library-path '("/home/harwiltz/zettelkasten/"))
(setq bibtex-completion-bibliography '("/home/harwiltz/zettelkasten/sources.bib"))
(setq bibtex-completion-pdf-open-function 'helm-open-file-with-default-tool)
(setq bibtex-completion-notes-path "/home/harwiltz/zettelkasten/paper-notes")
(setq bibtex-completion-notes-template-multiple-files
      "#+TITLE: [Notes] ${title}
#+AUTHOR: ${author-or-editor}

#+BEGIN_SRC bibtex
@${=type=}{
  title={${title}},
  author={${author}},
  year={${year}}
}
#+END_SRC")

;; org-mode stuff
(eval-after-load 'org '(add-to-list 'org-file-apps '("\\.pdf\\'" . emacs)))
(setq org-latex-create-formula-image-program 'dvipng)
(setq org-todo-keywords '((sequence "TODO" "IN PROGRESS" "|" "DONE" "PASS")))
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t) (haskell . t) (latex . t)))
(setq bibtex-completion-library-path "~/zettelkasten/library")

;; org-roam stuff
(defun harwiltz/init-org-roam ()
  (message "Initializing org roam stuff...")
  (require 'org-roam-protocol)
  (require 'org-agenda)
  (setq org-roam-directory "~/zettelkasten")
  (setq org-roam-task-dir (concat org-roam-directory "/backlog"))
  (setq org-roam-file-exclude-regexp "-index.org")
  (setq org-roam-graph-extra-config '(("bgcolor" . "grey12")))
  (setq org-roam-graph-edge-extra-config '(("color" . "grey42")))
  (setq org-roam-graph-node-extra-config '(("shape" . "rect")
                                           ("style" . "filled")
                                           ("fillcolor" . "gray50"))))

(add-hook 'after-init-hook
 (lambda ()
  (progn
    (install-missing-packages)
    (require 'assemble)
    (require 'parc.el)
    (harwiltz/init-latex)
    (harwiltz/init-org-roam)
    (message "Syncing org-roam db...")
    (org-roam-db-autosync-mode)
    (message "Finished initializing org-roam")
    (load-file "/home/harwiltz/.emacs.d/arxiv-sources.el")
    (require 'deft)
    (add-to-list 'default-frame-alist `(font . ,harwiltz/font))
    (message "about to load themes...")
    (load-theme harwiltz/light-theme t harwiltz/use-dark-theme)
    (message "loaded light theme")
    (load-theme harwiltz/dark-theme t (not harwiltz/use-dark-theme))
    (message "loaded dark theme"))))

(global-set-key (kbd "C-c j") (lambda () (interactive) (org-roam-capture)))
;;(define-key org-agenda-mode-map (kbd "C-c p") 'harwiltz/process-backlog-task)
;;(define-key org-agenda-mode-map (kbd "C-c q") 'harwiltz/unprocess-task)
(global-set-key (kbd "<f1>") (lambda () (interactive) (org-agenda nil "h")))


(setq harwiltz/current-mission "mission")
(setq org-clock-report-include-clocking-task t)
(setq org-agenda-start-with-clockreport-mode t)

(defun scrum-clocktable-write (&rest args)
  "Custom clocktable formatter for placing the
Effort column next to the Time column."
  (apply #'org-clocktable-write-default args)
  (save-excursion
    (forward-char)                ;; [File] Effort Headline Time
    (org-table-next-field)        ;; File [Effort] Headline Time
    (org-table-move-column-right) ;; File Headline [Effort] Time
    ))

(setq org-agenda-clockreport-parameter-plist
      '(:link t :maxlevel 2 :narrow 80 :properties ("Effort") :formatter scrum-clocktable-write))

(defun harwiltz/process-backlog-task ()
  (interactive)
  (org-agenda-priority)
  (org-agenda-set-effort)
  (org-agenda-deadline nil)
  (org-agenda-set-tags "processed" 'on)
  (when (member "inbox" (org-get-at-bol 'tags))
    (org-agenda-refile nil
		       `(nil
			 ,(concat org-roam-task-dir "/" harwiltz/current-mission ".org")
			 nil
			 nil))))

(defun harwiltz/unprocess-task ()
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'org-agenda-deadline))
  (org-agenda-priority)
  (org-agenda-set-tags "processed" 'off))

;; org-reveal stuff
(setq org-reveal-root "file:///home/harwiltz/reveal")

;; org-journal stuff
(setq org-journal-dir "/home/harwiltz/research/backlog/journal")
(setq org-journal-date-format "%A, %d %B %Y")

;; shell stuff
(setq explicit-shell-file-name "/bin/bash")
(setq multi-term-program "/bin/bash")

(setq harwiltz/latex-scale 1.3)
(defun org-latex-scale (scale)
  (interactive "nEnter scale: ")
  (when scale
    (setq harwiltz/latex-scale scale))
  (setq org-format-latex-options
	(plist-put org-format-latex-options
		   :scale harwiltz/latex-scale)))

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
  (insert (concat "#+NAME: eq:" label "\n"))
  (insert "\\begin{equation}\n\n")
  (insert "\\end{equation}")
  (forward-line -1)
  (beginning-of-line))

(defun insthm (type header)
  "Insert text block with header"
    (interactive "sBlock type: \nsHeader: ")
    (insert "#+ATTR_LATEX: :options [" header "]\n")
    (insert "#+NAME: " header "\n")
    (insert "#+begin_" (downcase type) "\n")
    (insert "#+end_" (downcase type) "\n")
    (forward-line -2)
    (end-of-line))

(defun insproof ()
  (interactive)
  (save-excursion
    (insert "#+begin_proof\n\n#+end_proof\n"))
  (forward-line))

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

;; Auto email agenda
(require 'json)
(defun auto-send-mail (from to &optional subject text html from-name to-name)
  (let* ((api_key    (getenv "MAILJET_API_KEY"))
	 (secret_key (getenv "MAILJET_SECRET_KEY"))
	 (msgdata
	  `((From     . ((Email . ,from) (Name . ,from-name)))
	    (To       . (((Email ., to) (Name . ,to-name))))
	    (Subject  . ,subject)
	    (TextPart . ,text)
	    (HTMLPart . ,html)))
	 (data `((Messages . ,(list msgdata))))
	 (rqst
	  (list "curl -s -X POST"
		"--user" (concat api_key ":" secret_key)
		"https://api.mailjet.com/v3.1/send"
		"-H 'Content-Type: application/json'"
		"-d" (prin1-to-string (json-encode data)))))
    (shell-command (mapconcat 'identity rqst " "))))

(setq harwiltz/agenda-export-path "~/org-agenda.html")
(setq harwiltz/default-agenda-recipient "wiltzerh@gmail.com")

(defun export-agenda (&optional template)
  (with-temp-buffer
    (org-agenda nil (or template "h"))
    (org-agenda-write harwiltz/agenda-export-path)))

(defun send-agenda (&optional to template)
  (interactive "sEmail to: " "sTemplate: ")
  (export-agenda (or template nil))
  (with-temp-buffer
    (append-file-to-buffer harwiltz/agenda-export-path)
    (auto-send-mail "emacs-vandelay@protonmail.com"
		    (or to harwiltz/default-agenda-recipient)
		    "Org Agenda Update"
		    nil
		    (buffer-string)
		    "Your Worst Enemy"
		    "Harley")))


(defun buffer-paste (&optional endpoint)
  (interactive)
  (let* ((dest (or endpoint "https://dpaste.org/api/"))
	 (file (make-temp-file "paste-content-"))
	 (rqst (list "curl --silent"
		     "-F 'format=default'"
		     (concat "-F 'content=<" file "'")
		     dest))
	 (cmd (mapconcat 'identity rqst " "))
	 (fp (write-region (point-min) (point-max) file))
	 (res (shell-command-to-string cmd)))
    (and (message res) (setq last-paste-url res) (delete-file file) res)))
		     
	       
(unless (boundp 'send-agenda-automatically)
  (setq send-agenda-automatically nil))

(run-at-time "11am" (* 60 60 4)
	     (lambda () (interactive)
	       (when send-agenda-automatically (send-agenda))))


;; Hooks
(require 'display-line-numbers)

(defcustom display-line-numbers-exempt-modes
  '(org-agenda-mode eshell-mode helm-mode)
  "Modes for which no line numbers should be rendered."
  :group 'display-line-numbers
  :type 'list
  :version "green")

(defun display-line-numbers--turn-on ()
  (unless (or (minibufferp)
	      (member major-mode display-line-numbers-exempt-modes))
    (display-line-numbers-mode)))

;; (global-display-line-numbers-mode)

(add-hook 'doc-view-mode-hook
	  (lambda ()
	    (message "Turning off line numbers")
	    (display-line-numbers-mode -1)))

(add-hook 'org-mode-hook
	  (lambda ()
	    (org-indent-mode)
	    (auto-fill-mode)
	    (org-bullets-mode)
	    (display-line-numbers-mode)
	    (hl-line-mode)
	    (setq org-hide-emphasis-markers t)
	    (org-latex-scale harwiltz/latex-scale)
	    (org-hide-block-all)))

;; Hatch stuff
(require 'hatch-button)
(global-set-key (kbd "<f2>") 'hatch-reset)

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
 '(linum-format " %7i ")
 '(package-selected-packages
   '(magit fzf deft org-tree-slide epresent yasnippet nix-mode ox-hugo org-roam-ui doom-themes dracula-theme gruvbox-theme helm-bibtex julia-repl julia-mode kotlin-mode sublime-themes request org-roam ox-reveal scala-mode dash-functional org-journal latex-preview-pane auctex markdown-preview-mode markdown-mode yaml-mode org-bullets org-re-reveal-ref dash org-ref base16-theme afternoon-theme inkpot-theme htmlize ample-theme haskell-mode multi-term spacemacs-theme evil))
 '(pdf-view-midnight-colors '("#b2b2b2" . "#292b2e"))
 '(safe-local-variable-values
   '((assemble-pdf-beamer . t)
     (default-assemble-target . "dev-env.pdf")))
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
 '(default ((t (:background nil))))
 '(mode-line ((t (:background "#2c2c2c" :foreground "#959697" :box (:line-width 1 :color "black" :style pressed-button)))))
 '(org-level-1 ((t (:height 1.5 :weight bold))))
 '(org-level-2 ((t (:height 1.4 :weight bold))))
 '(org-level-3 ((t (:height 1.3 :weight bold))))
 '(org-level-4 ((t (:height 1.2 :weight bold))))
 '(org-level-5 ((t (:height 1.1 :weight bold)))))

(require 'evil)
(evil-set-initial-state 'deft-mode 'emacs)
(evil-mode 1)

(require 'mu4e)

(defun org-export-pdf-then-open()
  (org-open-file (org-latex-export-to-pdf)))
(defun org-auto-export-on() "auto export to pdf when saving an org file" (interactive)
       (when (eq major-mode 'org-mode) (add-hook 'after-save-hook 'org-export-pdf-then-open t t) ) )
(put 'list-timers 'disabled nil)
