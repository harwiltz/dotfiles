(provide 'parc.el)

(defvar parcel-sepchar "-")
(defvar parcel-parent-tree-max-depth 5)

(defun parcel-add-zettel (title)
  "Add a zettel at the current level"
  (interactive "sTitle: ")
  (let* ((base-dir default-directory)
	 (filename (file-relative-name (buffer-file-name) base-dir))
	 (cur-point (save-excursion
		      (ignore-errors
			(org-back-to-heading)
			(point))))
	 (next-point (save-excursion
		       (org-forward-heading-same-level 1 t)
		       (point)))
	 (cur-id (org-entry-get nil "CUSTOM_ID"))
	 (new-id (if cur-id
		     (if (= cur-point next-point)
			 (parcel-create-heading title cur-id)
		       (parcel-create-subheading title cur-id))
		   (parcel-create-heading title
					  (concat (call-interactively 'parcel-tag-prompt)
						  "-0"))))
	 (parents (save-excursion (parcel-get-parents))))
    (if (one-window-p) (split-window-horizontally) (other-window 1))
    (find-file (concat base-dir new-id ".org"))
    (parcel-init-zettel title new-id parents filename)))

(defun parcel-init-zettel (title id parents filename)
  "Populate blank new zettel with metadata"
  (insert "#+STARTUP: indent\n")
  (insert (concat "#+TITLE: " title "\n"))               ;; this is simply going to be the <title>
  (insert (concat "#+AUTHOR: " (user-login-name) "\n"))
  (insert "#+OPTIONS: toc:nil num:nil title:nil\n\n")    ;; suppress auto-generated header for title
  (parcel-insert-parents-header parents filename)
  (insert "\n")
  (insert "#+BEGIN_EXPORT html\n")
  (insert "<div class=\"parcel\">\n")
  (insert (concat "<h1 class=\"parcel-title\">" (parcel-describe-zettel-html title id) "</h1>\n"))
  (insert "#+END_EXPORT\n")
  (insert "Hello, Newman\n\n")
  (insert "#+BEGIN_EXPORT html\n")
  (insert "</div>\n")
  (insert "#+END_EXPORT\n")
  (forward-line -5)
  (beginning-of-line))

(defun parcel-get-parents (&optional cur-depth)
  "Get list of parents of curent zettel"
  (let* ((depth (or cur-depth 0))
	 (remaining (- parcel-parent-tree-max-depth depth))
	 (parent (ignore-errors
		     (outline-up-heading 1)
		     `((title . ,(org-entry-get nil "TITLE"))
		       (id . ,(org-entry-get nil "CUSTOM_ID"))))))
    (if (> remaining 0)
	(and parent (cons parent (parcel-get-parents (+ depth 1))))
      nil)))
      
(defun parcel-insert-parents-header (parents filename)
  "Insert list of parents in parcel-parents block"
  (insert "#+BEGIN_parcel-parents\n")
  (insert (concat "- [[file:" filename "][" (index-name filename) "]]\n"))
  (mapcar (lambda (parent)
	    (pcase parent (`((title . ,ti) (id . ,i))
			   (insert (concat "- [[file:" i ".org][" (parcel-describe-zettel ti i) "]]\n")))))
	  (nreverse parents))
  (insert "#+END_parcel-parents\n"))

(defun parcel-create-heading (title id)
  "Create a new zettel at the current level"
  (let ((new-id (parcel-incr-id id)))
    (org-insert-heading-respect-content)
    (parcel-insert-heading title new-id)
    new-id))

(defun parcel-create-subheading (title id)
  "Create a new zettel under the current one"
  (let ((new-id (concat (string-join (split-string id parcel-sepchar)) "-" (parcel-init-sub-id id))))
    (org-insert-heading-respect-content)
    (org-do-demote)
    (parcel-insert-heading title new-id)
    new-id))

(defun parcel-tag-prompt (tag)
  (interactive "sLooks like a clean slate. Enter identifying tag: ")
  tag)

(defun parcel-insert-heading (title id)
  (insert (concat "[[" (concat "file:" id ".org") "][ =#" id "= ]] " title))
  (save-excursion
    (org-set-property "CUSTOM_ID" id)
    (org-set-property "TITLE" title)))

(defun parcel-incr-id (id)
  "Increment a given id"
  (if id
      (let* ((parts (split-string id parcel-sepchar))
	     (prefix (car parts))
	     (suffix (car (last parts))))
	(if (string-match-p "^[a-z]+$" suffix)
	    (if (string-match-p "z$" suffix)
		(parcel-mk-id prefix (concat (string-trim-right suffix ".") "aa"))
	      (parcel-mk-id prefix (byte-to-string
				    (+ (string-to-char (last suffix)) 1))))
	  (parcel-mk-id prefix (number-to-string (+ (string-to-number suffix) 1)))))
    "1"))

(defun parcel-mk-id (prefix suffix)
  (concat prefix parcel-sepchar suffix))

(defun parcel-init-sub-id (id)
  (if (string-match-p "[a-z]$" id)
      "1"
    "a"))

(defun parcel-describe-zettel (title id)
  (concat "=#" id "= " title))

(defun parcel-describe-zettel-html (title id)
  (concat "<code>#" id "</code> " title))

(defun index-name (filename)
  "Converts the file name to a suitable index title"
  (let* ((file-base-name (string-remove-suffix ".org" filename))
	 (words (split-string file-base-name "-")))
    (mapconcat 's-capitalize title-words " ")))

(require 'assemble)
(setq parcel-builddir "build")
(setq parcel-assets-path "~/.emacs.d/lisp/parcel")
(setq parcel-css-name "parcel.css")
(setq parcel-index-css-name "parcel-index.css")
(setq parcel-css (concat parcel-assets-path "/" parcel-css-name))
(setq parcel-fonts
      '("SourceSerifFont"
	"RobotoSlabFont"))

(defun parcel-assemble ()
  (interactive)
  (parcel-build-init)
  (let* ((file-base-name (car (last (split-string (buffer-file-name) "/"))))
	 (target-path (concat parcel-builddir "/"
			      (concat (string-remove-suffix "org" file-base-name) "html"))))
    (defile "parcel"
      (lambda ()
	(insert-css parcel-css-name)
	(and (string-suffix-p "-index.org" file-base-name)
	     (insert-css parcel-index-css-name))
	(append-file-to-buffer file-base-name)
	(assemble-html target-path)))))

(defun parcel-build-init ()
  (shell-command (concat "mkdir -p " parcel-builddir))
  (shell-command (format "cp %s %s/"
			 parcel-css
			 parcel-builddir))
  (shell-command (format "cp %s %s/"
			 (concat parcel-assets-path "/" parcel-index-css-name)
			 parcel-builddir))
  (mapcar
   (lambda (font)
     (shell-command (format "cp -R %s %s/"
			    (concat parcel-assets-path "/" font)
			    parcel-builddir)))
   parcel-fonts))
