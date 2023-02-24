(require 'helm-bibtex)
(setq bibtex-completion-additional-search-fields '(tags))

(push
 '("online" "Online" (("author") ("title")) (("year")) (("note")))
 bibtex-entry-alist)

(setq bibtex-completion-display-formats
      '((t . "${author:24} ${title:*} ${tags:16} ${year:4} ${=has-pdf=:1}${=has-note=:1}")))

(setq harwiltz/bibtex-tags
      '(rl    ; reinforcement learning
	rlt   ; rl theory
	exp   ; exploration in rl
	mbrl  ; model-based rl
	gen   ; generative models
	sde   ; stochastic differential equations
	ct    ; continuous time
	ctrl  ; control theory
	bayes ; bayesian models
	risk  ; risk-sensitivity
	game  ; game theory / multi-agent
	drl   ; distributional rl
	mab   ; bandits
	prob  ; probability
	ana   ; analysis
	opt   ; optimization
	cls   ; classic paper
	repr  ; representation learning
	mfg   ; mean-field games
	hrl   ; hierarchical rl
	trans ; transfer learning
	nn    ; neural nets
	meta  ; meta learning
	robo  ; robotics
	bench ; benchmarking
	ml    ; machine learning
	la    ; linear algebra
	alg   ; algebra
	pde   ; partial differential equations
	))

(defun bibtex-add-tags (keys)
  "Add tags to bibtex entries"
  (dolist (key keys)
    (message key)
    (let* ((tags (completing-read-multiple "Tag(s): " harwiltz/bibtex-tags))
	   (entry (bibtex-completion-get-entry key))
	   (initial-tags-line (bibtex-completion-get-value "tags" entry))
	   (initial-tags (and initial-tags-line (split-string initial-tags-line " ")))
	   (all-tags (delete-dups (append initial-tags tags))))
      (save-window-excursion
	(bibtex-completion-show-entry (list key))
	(unless initial-tags (bibtex-make-field "tags" t))
	(bibtex-set-field "tags" (mapconcat 'identity all-tags " "))
	(save-buffer)))))

(defun test ()
  (interactive)
  (message (completing-read-multiple "Tag(s): " harwiltz/bibtex-tags)))

(helm-bibtex-helmify-action bibtex-add-tags helm-bibtex-add-tags)
(helm-add-action-to-source
 "Add tag(s)" 'helm-bibtex-add-tags helm-source-bibtex)
