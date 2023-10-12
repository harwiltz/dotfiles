(require 'biblio-arxiv)

(setq harwiltz-arxiv/url (biblio-arxiv--url "distributional rl"))

(defun harwiltz-arxiv/add-source ()
  "Add the source given by the metadata at point in biblio-arxiv-lookup
to the bibtex-completion library and the parcel bibliography"
  (interactive)
  (let* ((metadata   (biblio--selection-metadata-at-point))
	 (bibtex-raw (biblio-arxiv--build-bibtex-1 metadata))
	 (key        (harwiltz-arxiv/mkey metadata))
	 (bibtex     (harwiltz-arxiv/format-bibtex bibtex-raw key)))
    (harwiltz-arxiv/add-source--install bibtex key metadata)))

(defun harwiltz-arxiv/add-source--install (bibtex key metadata)
  "Download pdf to bibtex-completion library and add bibtex data
to the parcel bibliography files.

BIBTEX is a string containing the bibtex of the given entry.
KEY is the id of the entry (see harwiltz-arxiv/mkey).
METADATA is the alist of metadata from biblio-arxiv-lookup. It must contain
an identifier field (the arxiv id) and a title field."
  (let-alist metadata
    (parcel-add-sources-entry bibtex org-roam-directory)
    (parcel-add-bibliography-entry bibtex .identifier .title org-roam-directory)
    (let ((path (concat bibtex-completion-library-path "/" key ".pdf"))
	  (url-user-agent "Mozilla/5.0 (X11; Linux x86_64)")
	  (url (biblio-arxiv--pdf-url .identifier)))
      (url-copy-file url path 1))))

(defun harwiltz-arxiv/mkey (metadata)
  "Generate the key for a bibtex entry.
METADATA is the alist of metadata from biblio-arxiv-lookup. It must contain
an identifier field (the arxiv id) and a title field.
If we are adding the paper
\"A Distributional Perspective on Reinforcement Learning\" by
Marc G. Bellemare et. al from 2017, the key returned should be
bellemare2017adistributional."
  (let-alist metadata
    (let* ((authors    (seq-filter 'identity .authors))
	   (firstauthor(car authors))
	   (names      (split-string firstauthor " "))
	   (name       (downcase (car (nreverse names))))
	   (titlewords (split-string .title " "))
	   (firstword  (car titlewords)))
      (if (< (length firstword) 4)
	  (concat name .year (downcase firstword) (downcase (car (cdr titlewords))))
	(concat name .year (downcase firstword))))))

(defun harwiltz-arxiv/format-bibtex (bibtex key)
  "Substitute the key placeholder in BIBTEX from biblio-arxix--build-bibtex-1
with KEY."
  (replace-regexp-in-string "NO_KEY" key bibtex t))

(define-key biblio-selection-mode-map "B" 'harwiltz-arxiv/add-source)
