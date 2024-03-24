(use-package org
  :config
  (progn
    (add-to-list 'org-file-apps
		 '("\\.pdf\\'" . "zathura %s"))
    (custom-theme-set-faces
     'user
     '(org-block ((t (:inherit fixed-pitch))))
     '(org-code ((t (:inherit (shadow fixed-pitch)))))
     '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
     '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
     '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
     '(org-property-value ((t (:inherit fixed-pitch))) t)
     '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
     '(org-table ((t (:inherit fixed-pitch))))
     '(org-tag ((t (:inherit (shadow fixed-pitch)))))
     '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))
    (setq org-hide-emphasis-markers t
	  org-todo-keywords '((sequence "TODO" "IN PROGRESS" "WAITING" "|" "DONE" "PASS")
			      (sequence "BACKLOG" "NEXT" "PENDING" "|" "FINISHED"))
	  harwiltz/org-latex-packages-alist '(("" "harwiltzmath" t)
					      ("" "harwiltzdraft" t))
	  org-latex-packages-alist harwiltz/org-latex-packages-alist))
  :hook ((org-mode . org-indent-mode)
	 (org-mode . auto-fill-mode)
	 (org-mode . (lambda () (org-latex-scale nil)))))

(setq harwiltz/org-pretty nil)

(defun harwiltz/org-pretty-hook ()
  (variable-pitch-mode)
  (visible-line-mode))

(defun org-toggle-pretty ()
  (interactive)
  (if harwiltz/org-pretty
      (remove-hook 'org-mode-hook 'harwiltz/org-pretty-hook)
    (add-hook 'org-mode-hook 'harwiltz/org-pretty-hook))
  (setq harwiltz/org-pretty (not harwiltz/org-pretty)))

(setq harwiltz/latex-scale 1.3)
(defun org-latex-scale (scale)
  (interactive "nEnter scale: ")
  (when scale
    (setq harwiltz/latex-scale scale))
  (setq org-format-latex-options
	(plist-put org-format-latex-options
		   :scale harwiltz/latex-scale)))

(defun insderiv ()
  "Insert untagged aligned equation"
  (interactive)
  (insert "\\begin{align*}\n")
  (insert "\\end{align*}")
  (forward-line -1)
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

;;;; FOR VARIABLE PITCH FONT
;; (use-package org
;;   :config
;;   (progn
;;     (setq org-hide-emphasis-markers t)
;;     (custom-theme-set-faces
;;      'user
;;      '(org-block ((t (:inherit fixed-pitch))))
;;      '(org-code ((t (:inherit (shadow fixed-pitch)))))
;;      '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
;;      '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
;;      '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
;;      '(org-property-value ((t (:inherit fixed-pitch))) t)
;;      '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
;;      '(org-table ((t (:inherit fixed-pitch))))
;;      '(org-tag ((t (:inherit (shadow fixed-pitch)))))
;;      '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))))
;;   :hook ((org-mode . org-indent-mode)
;; 	 (org-mode . variable-pitch-mode)
;; 	 (org-mode . visual-line-mode)))
