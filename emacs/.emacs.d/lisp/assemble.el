(provide 'assemble)

(require 'ox)
(require 'ox-latex)
(require 'seq)
(require 'subr-x)

(defvar assemble-file "assemb.el")

(defmacro assemble-target (name depending on deps &rest body)
  "Defines a build target with specified dependencies.

NAME is the name of the build target, which can be executed with
(assemble \"NAME\").  Additionally, NAME should correspond to the name
of the file being produced by this target.

DEPS is a list of dependencies for this target.

BODY is the body of the build target, which is responsible for
building NAME.

For example, suppose assemb.el contains

(assemble-target \"foo.html\" depending on '(\"foo.org\")
                 (defile \"foo\"
                    (append-file-to-buffer \"foo.org\")
                    (assemble-html \"test.html\")))

Then, executing (assemble \"foo.html\") will export foo.org to html if
foo.org has been modified since the last time it was built."
  `(defun ,(intern name) ()
     nil
     nil
     (progn
      (message (concat "[ . ] " ,name))
      (if (seq-reduce
	   (lambda (acc x) (or acc x))
	   (mapcar (lambda (dep) (assemble-ifchanged ,name dep))
		   ,deps)
	   nil)
	  (progn (message (concat "[>>>] " ,name)) ,@body t)
	nil))))

(defmacro assemble-wildcard (extension depending on deps &rest body)
  `(defun ,(intern (concat "_" extension)) (filename)
     nil
     nil
     (progn
       (message (concat "[ * ] " filename))
       (if (seq-reduce
	    (lambda (acc x) (or acc x))
	    (mapcar (lambda (dep) (assemble-ifchanged filename dep))
		    (resolve-wildcard-deps ,deps filename))
	    nil)
	   (let ((_filename (file-name-sans-extension filename)))
	     (progn (message (concat "[>>>] " filename)) ,@body t))
	 nil))))

(defun resolve-wildcard-deps (deps file)
  (mapcar (lambda (dep)
	    (let* ((dep-name (symbol-name dep)))
	      (if (string-prefix-p "_" dep-name)
		  (intern (concat (file-name-sans-extension file) "."
				  (string-remove-prefix "_" dep-name)))
		dep)))
	  deps))

(defun assemble-ifchanged (target dep)
  "Assemble a build dependency and report if its parent needs to be assembled"
  (if (string-equal target (symbol-name dep))
      (and (message (concat "[XXX] Fatal Error :: Cyclic dependency detected: "
			    target " depends on itself!"))
	   nil)
    (let* ((dep-path (symbol-name dep))
	    (extension (file-name-extension dep-path))
	    (wildcard (intern (concat "_" extension))))
	(if (or (fboundp dep) (fboundp wildcard))
	    (and (if (fboundp dep) (funcall dep) (funcall wildcard dep-path))
		(and (file-newer-than-file-p dep-path target) t))
	(and (file-exists-p dep-path) (file-newer-than-file-p dep-path target))))))

(defun assemble (target)
  "Assemble a given target"
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

(defmacro defile (target-name &rest body)
  `(with-temp-buffer
     (rename-buffer (concat ,target-name "---assembling"))
     (message (concat "Assembling " ,target-name "..."))
     ,@body))

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
  (let ((out-path (if out-dir
		      (concat out-dir "/" file-name)
		    file-name)))
    (simple-org-export
     'latex
     'org-latex-compile
     out-path)))

(defun assemble-latex-bibtex (file-base-name &optional out-dir)
  "Produce latex pdf and compile bibtex from buffer"
  (require 'org-ref)
  (let* ((pdflatex-cmd (format "pdflatex %s %s.tex"
			       (if out-dir (concat "-output-directory=" out-dir) "")
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
