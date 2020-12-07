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

(defun simple-org-export (type export-fn file-name)
  "Wrapper around org-export-to-file with simple defaults"
  (message (concat ":: Exporting to " file-name "..."))
  (org-export-to-file
      type
      file-name
    nil nil nil nil nil
    export-fn))

(defun assemble-latex (file-name)
  "Produce latex pdf from buffer"
  (simple-org-export
   'latex
   'org-latex-compile
   file-name))

(defun assemble-html(file-name)
  "Produce HTML from buffer"
  (simple-org-export 'html nil file-name))
