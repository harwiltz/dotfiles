(use-package org-chef :ensure t)

(use-package org
  :config
  (setq org-hide-emphasis-markers t
        org-todo-keywords '((sequence "TODO" "IN PROGRESS" "WAITING" "|" "DONE" "PASS")
                            (sequence "BACKLOG" "NEXT" "PENDING" "|" "FINISHED"))
        org-journal-dir "~/journal"
        org-preview-latex-default-process 'dvisvgm
        org-cookbook-file "~/cookbook.org"
        org-capture-templates `(("c" "Cookbook" entry (file ,org-cookbook-file)
                                 "%(org-chef-get-recipe-from-url)"
                                 :empty-lines 1)
                                ("m" "Manual Cookbook" entry (file ,org-cookbook-file)
                                 "* %^{Recipe title}\n:PROPERTIES:\n:source-url:\n:servings:\n:prep-time:\n:cook-time:\n:ready-in:\n:END:\n** Ingredients [/]\n %?\n** Directions\n\n")))
  :hook ((org-mode . org-indent-mode)
         (org-mode . auto-fill-mode)
         (org-mode . (lambda () (org-latex-scale nil)))))

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

(defun insthm ()
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
