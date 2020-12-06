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
      (funcall fn out_dir r))))
