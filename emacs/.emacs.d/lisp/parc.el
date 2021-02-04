(provide 'parc.el)

(require 'dash)
(require 's)
(require 'seq)

(defvar parcel-sepchar "-")
(defvar parcel-parent-tree-max-depth 5)

;; parcel-split-window: Controls splitting of window when creating zettel
;; 1   : split horizontally
;; 2   : split vertically
;; nil : don't split, just open new buffer
(or (boundp 'parcel-split-window)
    (setq parcel-split-window 1))

(or (boundp 'parcel-pagerank-reset-prob)
    (setq parcel-pagerank-reset-prob 0.15))

(or (boundp 'parcel-pagerank-runs)
    (setq parcel-pagerank-runs 200))

(or (boundp 'parcel-stochastic-backlinks)
    (setq parcel-stochastic-backlinks nil))

(or (boundp 'parcel-num-backlink-candidates)
    (setq parcel-num-backlink-candidates 20))

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
    (when parcel-split-window
	 (if (one-window-p)
	     (if (eq 1 parcel-split-window)
		 (split-window-horizontally)
	       (split-window)))
	   (other-window 1))
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
  (insert "\n")
  (require 'org-ref)
  (insert "#+BEGIN_bibliography\n")
  (insert "bibliography:sources.bib\n")
  (insert "#+END_bibliography\n")
  (forward-line -9)
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
    (mapconcat 's-capitalize words " ")))

(defun parcel-add-reference ()
  "Prompt user to add reference"
  (interactive)
  (let ((basedir default-directory))
    (switch-to-buffer (get-buffer-create "*parcel add reference*"))
    (lisp-interaction-mode)
    (erase-buffer)
    (insert ";; Paste bibtex reference below this line\n\n\n")
    (insert ";; Execute the following code to add the reference:\n\n")
    (insert (format "(parcel-commit-reference \"%s\")" basedir))
    (goto-char 0)
    (forward-line 1)))

(defun parcel-commit-reference (basedir)
  "Install the reference from the scratch buffer"
  (goto-char 0)
  (forward-line 1)
  (let* ((match-source-type "@[a-z]+")
	 (match-alnum-string "[a-zA-Z0-9]+")
	 (match-id-string (concat match-alnum-string "[a-zA-Z0-9-:]+"))
	 (match-arbitrary-lines "\\(.*\n\\)*")
	 (match-whitespace "\\s-*")
	 (match-attr-text ".*"))
    (re-search-forward (concat match-source-type
			       "{"
			       "\\(" match-id-string "\\)" ","
			       match-arbitrary-lines
			       match-whitespace "title"
			       match-whitespace "="
			       match-whitespace "{"
			       "\\(" match-attr-text "\\)"
			       "},"
			       match-arbitrary-lines
			       "}"))
    (let ((bibtex (match-string 0))
	  (id     (match-string 1))
	  (title  (match-string 3)))
      (parcel-add-sources-entry bibtex basedir)
      (parcel-add-bibliography-entry bibtex id title basedir))))

(defun parcel-add-sources-entry (bibtex basedir)
  "Add bibtex entry to sources.bib"
  (append-to-file (concat bibtex "\n")
		  nil
		  (concat basedir "/sources.bib")))

(defun parcel-add-bibliography-entry (bibtex id title basedir)
  "Add section with bibtex entry to bibliography"
  (let ((buf (find-file-noselect (concat basedir "/bibliography.org"))))
    (with-current-buffer buf
      (save-excursion
	(end-of-buffer)
	(insert (concat "* " title " [" id "]\n"))
	(org-set-property "CUSTOM_ID" id)
	(insert "#+BEGIN_SRC bibtex\n")
	(insert bibtex)
	(insert "\n")
	(insert "#+END_SRC\n"))
      (save-buffer))))

(require 'assemble)
(setq parcel-builddir "build")
(setq parcel-res-dir "res")
(setq parcel-assets-path "~/.emacs.d/lisp/parcel")
(setq parcel-css-name "parcel.css")
(setq parcel-index-css-name "parcel-index.css")
(setq parcel-master-index-css-name "parcel-master-index.css")
(setq parcel-bibliography-css-name "parcel-bibliography.css")
(setq parcel-css-files
      `(,parcel-css-name
	,parcel-index-css-name
	,parcel-master-index-css-name
	,parcel-bibliography-css-name))
(setq parcel-fonts
      '("SourceSerifFont"
	"RobotoSlabFont"))
(setq zettel-regex "\\.org$")

(defun parcel-assemble-all ()
  "Assemble the entire zettelkasten"
  (interactive)
  (parcel-build-init)
  (parcel-assemble-index)
  (let ((files (directory-files "." nil ".org$")))
    (mapcar 'parcel-assemble files)))

(defun parcel-assemble (&optional filename)
  (interactive)
  (require 'org-ref)
  (parcel-build-init)
  (let* ((file-base-name (or filename (car (last (split-string (buffer-file-name) "/")))))
	 (target-path (concat parcel-builddir "/"
			      (concat (string-remove-suffix "org" file-base-name) "html"))))
    (when (file-newer-than-file-p file-base-name target-path)
      (let ((links (and (parcel-zettel? (file-name-nondirectory file-base-name))
			(parcel-pagerank (concat (file-name-nondirectory file-base-name))
					 parcel-pagerank-reset-prob
					 parcel-pagerank-runs
					 parcel-stochastic-backlinks
					 parcel-num-backlink-candidates))))
	(defile "parcel"
	  (message (concat ">>> " file-base-name))
	  (insert-css parcel-css-name)
	  (and (string-suffix-p "-index.org" file-base-name)
	       (insert-css parcel-index-css-name))
	  (and (string-suffix-p "bibliography.org" file-base-name)
	       `(,(insert (concat "#+TITLE: " (user-login-name) "'s Bibliography\n"))
		 ,(insert (concat "#+AUTHOR: " (user-login-name) "\n"))
		 ,(insert "#+OPTIONS: toc:nil num:nil\n")
		 ,(insert-css parcel-bibliography-css-name)))
	  (insert "#+BEGIN_EXPORT html\n")
	  (append-file-to-buffer (concat parcel-assets-path "/navbar.html"))
	  (insert "#+END_EXPORT\n\n")
	  (append-file-to-buffer file-base-name)
	  (and links (parcel-insert-relevant-links links))
	  (assemble-html target-path))))))

(defun parcel-assemble-index (&optional out-name)
  "Build a master index page"
  (let ((index-files (directory-files "." nil "\\(-index\\|^bibliography\\)\\.org$"))
	(page-name (concat parcel-builddir "/" (or out-name "index.html"))))
    (defile "index"
      (insert-css parcel-css-name)
      (insert-css parcel-master-index-css-name)
      (insert (concat "#+TITLE: Zettelkasten of " (user-login-name) "\n"))
      (insert (concat "#+AUTHOR: " (user-login-name) "\n"))
      (insert "#+OPTIONS: toc:nil num:nil\n\n")
      (insert "#+BEGIN_EXPORT html\n")
      (append-file-to-buffer (concat parcel-assets-path "/navbar.html"))
      (insert "#+END_EXPORT\n\n")
      (insert "#+BEGIN_parcel-master-index-list\n")
      (mapcar
       (lambda (file)
	 (insert (concat "- [[file:" file "][" (index-name file) "]]\n")))
       index-files)
      (insert "#+END_parcel-master-index-list\n")
      (assemble-html page-name))))

(defun parcel-build-init ()
  (shell-command (concat "mkdir -p " parcel-builddir))
  (shell-command (format "cp -R %s %s/" parcel-res-dir parcel-builddir))
  (mapcar
   (lambda (css)
     (shell-command (format "cp %s %s/"
			    (concat parcel-assets-path "/" css)
			    parcel-builddir)))
   parcel-css-files)
  (mapcar
   (lambda (font)
     (shell-command (format "cp -R %s %s/"
			    (concat parcel-assets-path "/" font)
			    parcel-builddir)))
   parcel-fonts))

(defun parcel-insert-relevant-links (links)
  (save-excursion
    (parcel-remove-existing-relevant-links)
    (end-of-buffer)
    (insert "#+BEGIN_parcel-relevant-links\n")
    (mapcar (lambda (rel)
	      (pcase rel (`(,id . ,title)
			  (insert (concat
				   "- [[file:" id
				   "]["
				   (parcel-describe-zettel title (file-name-base id))
				   "]]\n")))))
	    links)
    (insert "#+END_parcel-relevant-links\n")))

(defun parcel-remove-existing-relevant-links ()
  (let ((bounds (org-element-map (org-element-parse-buffer)
		    'special-block (lambda (block)
				     (and (string= (org-element-property :type block)
						   "parcel-relevant-links")
					  `(,(org-element-property :begin block)
					    ,(org-element-property :end block)))))))
    (mapcar
     (lambda (b)
       (save-excursion
	 (goto-char (car b))
	 (set-mark-command nil)
	 (goto-char (pop (cdr b)))
	 (delete-forward-char 1)))
     (nreverse (delq nil bounds)))))

(defun parcel-pagerank (&optional file
				  reset-prob
				  num-runs
				  stochastic-backlinks
				  num-backlink-candidates)
  (let* ((filepath (or file (buffer-file-name)))
	 (p (or reset-prob parcel-pagerank-reset-prob))
	 (stochastic-backlinks? (or stochastic-backlinks parcel-stochastic-backlinks))
	 (num-stochastic-backlinks (or num-backlink-candidates parcel-num-backlink-candidates))
	 (backlinks (parcel-find-backlinks filepath
					   stochastic-backlinks?
					   num-stochastic-backlinks))
	 (runs (or num-runs parcel-pagerank-runs))
	 (relfile (file-name-nondirectory filepath))
	 (roots (cons relfile backlinks))
	 (chosen-roots (mapcar
			(lambda (n)
			  (nth (random (length roots)) roots))
			(number-sequence 1 runs)))
	 (geometric-samples (mapcar
			     (lambda (n)
			       (let* ((precision 10000)
				      (u (/ (float (random precision)) precision)))
				 (/ (log (- 1 u)) (log (- 1 p)))))
			     chosen-roots))
	 (path-lengths (mapcar 'ceiling geometric-samples))
	 (leaves (mapcar
		  (lambda (z) (pcase z (`(,r . ,l) (parcel-random-walk r l))))
		  (-zip-pair chosen-roots path-lengths)))
	 (filtered-leaves (seq-filter
			   (lambda (f)
			     (not (string= (car f) relfile)))
			   leaves))
	 (grouped-leaves (-group-by 'identity filtered-leaves)))
    (mapcar 'car
	    (--sort (> (length it) (length other))
		    grouped-leaves))))

(defun parcel-random-walk (root path-length)
  (let* ((has-visitor (get-buffer root))
	 (buf (or has-visitor (find-file-noselect root)))
	 (title (with-current-buffer buf (parcel-get-title)))
	 (links (and (> path-length 0) (with-current-buffer buf (parcel-get-linked-zettels))))
	 (num-links (and (> path-length 0) (length links)))
	 (dead-end (or (eq num-links 0) (eq path-length 0)))
	 (chosen-index (or dead-end (random (length links))))
	 (chosen (or dead-end (nth chosen-index links))))
    (progn (or has-visitor (kill-buffer root))
	   (if (not dead-end)
	       (parcel-random-walk chosen (- path-length 1))
	     `(,root . ,title)))))

(defun parcel-find-backlinks (&optional file stochastic stochastic-candidates)
  (let* ((filename (file-name-base (or file (buffer-file-name))))
	 (all-zettels (parcel-get-all-zettels))
	 (num-zettels-total (length all-zettels))
	 (candidates (if stochastic
			 (let ((num (or stochastic-candidates 20)))
			   (mapcar (lambda (n)
				     (nth (random num-zettels-total)
					  all-zettels))
				   (number-sequence 0 (- num-zettels-total 1))))
		       all-zettels)))
    (seq-reduce
     (lambda (acc zettel)
       (let* ((has-visitor (get-buffer zettel))
	      (buf (or has-visitor (find-file-noselect zettel)))
	      (links (with-current-buffer buf (parcel-get-linked-zettels))))
	 (progn
	   (unless has-visitor (kill-buffer buf))
	   (if (member (concat filename ".org") links)
	       (cons zettel acc)
	     acc))))
     (delq nil (delete-dups candidates))
     nil)))

(defun parcel-get-all-zettels (&optional dir)
  (let* ((directory (or dir "."))
	 (orgs (directory-files directory nil zettel-regex)))
    (seq-filter 'parcel-zettel? orgs)))

(defun parcel-zettel? (file)
  (and (string-suffix-p ".org" file)
       (not (string-suffix-p "-index.org" file))
       (not (string= "bibliography.org" file))))

(defun parcel-get-linked-zettels ()
  (save-excursion
    (let ((links (org-element-map (org-element-parse-buffer) 'link
		   (lambda (link)
		     (when (string= (org-element-property :type link) "file")
		       (org-element-property :path link))))))
      (seq-filter 'parcel-zettel? links))))

(defun parcel-get-title ()
  (save-excursion
    (goto-char 0)
    (re-search-forward "#\\+TITLE: \\(.+\\)$")
    (match-string 1)))
