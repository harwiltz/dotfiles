(provide 'assemble)

(require 'ox)
(require 'ox-latex)
(require 'subr-x)

(defvar assemble-file "assemb.el")

(defun assemble (target)
  (interactive "sTarget (nil): ")
  (let* ((dir (find-assemble-dir default-directory))
	 (load-path (cons dir load-path)))
    (message (concat "Found assemb.el: " dir "/" assemble-file))
    (load-file assemble-file)
    (build-target target)))

(defun find-assemble-dir (dir)
  (let ((dirf (string-remove-suffix "/" dir)))
    (if (file-exists-p (concat dirf "/" assemble-file))
	dirf
      (if (null dirf)
	  (error "No assemb.el file found")
	(find-assemble-file (file-name-directory dirf))))))

(defun build-target (&optional target &rest r)
  "Assemble a given target"
  (if (or (not (boundp 'target)) (not target) (eq (length target) 0))
      (default)
    (let ((out_dir "./")
	  (fn (intern target)))
      (funcall fn))))

(defun defile (target-name thunk)
  "Perform actions in a temporary file"
  (with-temp-buffer
    (rename-buffer (concat target-name "---assembling"))
    (message (concat "Assembling " target-name "..."))
    (funcall thunk)
    (message "DONE")))

(defun append-file-to-buffer (file)
  (insert-file-contents file)
  (goto-char (point-max)))

(defun simple-org-export (type export-fn file-name)
  "Wrapper around org-export-to-file with simple defaults"
  (message (concat ":: Exporting to " file-name "..."))
  (org-export-to-file
      type
      file-name
    nil nil nil nil nil
    export-fn))

(defun assemble-latex (file-name &optional out-dir)
  "Produce latex pdf from buffer"
  (simple-org-export
   'latex
   'org-latex-compile
   (concat out-dir "/" file-name)))

(defun assemble-latex-bibtex (file-base-name &optional out-dir)
  "Produce latex pdf and compile bibtex from buffer"
  (require 'org-ref)
  (let* ((pdflatex-cmd (format "pdflatex %s %s.tex"
			       (and out-dir (concat "-output-directory=" out-dir))
			       file-base-name))
	 (bibtex-cmd (format "bibtex %s"
			     (if out-dir (concat out-dir "/" file-base-name) file-base-name)))
	 (org-latex-pdf-process
	  `(,pdflatex-cmd
	    ,bibtex-cmd
	    ,pdflatex-cmd
	    ,pdflatex-cmd)))
    (assemble-latex (concat file-base-name ".tex") (and (boundp 'out-dir) out-dir))))

(defun assemble-latex-compile (file-list out-name)
  "Compile list of org files into a single pdf"
  (defile out-name
    (lambda ()
      (mapcar 'append-file-to-buffer
	      (mapcar (lambda (s) (concat s ".org")) file-list))
      (replace-links-with-references)
      (assemble-latex-bibtex out-name))))

(defun replace-links-with-references ()
  "Replace links to files with links to headers"
  (save-excursion
    (goto-char 0)
    (while (re-search-forward "file:" nil t)
      (replace-match "#"))))

(defun assemble-html(file-name)
  "Produce HTML from buffer"
  (simple-org-export 'html nil file-name))

(defun html-clean-org ()
  (save-excursion
    (goto-char 0)
    (flush-lines ":CUSTOM_ID:")
    (goto-char 0)
    (flush-lines "#\\+STARTU")))

(defun insert-css (file)
  (insert (concat "#+HTML_HEAD: " (make-css-link file) "\n")))

(defun make-css-link (file)
  (format "<link rel=\"stylesheet\" type=\"text/css\" href=\"%s\" />"
	  file))

(defun file-contents-if-exists (file-path)
  (if (and (boundp file-path) (symbol-value file-path))
      (get-file-as-string (symbol-value file-path))
    ""))

(defun get-file-as-string (file)
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))
