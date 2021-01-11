(provide 'parc.el)

(defvar parcel-sepchar "-")

(defun parcel-add-heading (title)
  "Add a zettel at the current level"
  (interactive "sTitle: ")
  (let* ((cur-point (save-excursion
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
							     "-0")))))
	 (find-file-other-window (concat new-id ".org"))))

(defun parcel-create-heading (title id)
  "Create a new zettel at the current level"
  (let ((new-id (parcel-incr-id id)))
    (save-excursion
      (org-insert-heading-respect-content)
      (parcel-insert-heading title new-id))
    new-id))

(defun parcel-create-subheading (title id)
  "Create a new zettel under the current one"
  (let ((new-id (concat (string-join (split-string id parcel-sepchar)) "-" (parcel-init-sub-id id))))
    (save-excursion
      (org-insert-heading-respect-content)
      (org-do-demote)
      (parcel-insert-heading title new-id))
    new-id))

(defun parcel-tag-prompt (tag)
  (interactive "sLooks like a clean slate. Enter identifying tag: ")
  tag)

(defun parcel-insert-heading (title id)
  (insert (concat "[[" (concat "file:" id ".org") "][#" id "]] " title))
  (org-set-property "CUSTOM_ID" id))

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
