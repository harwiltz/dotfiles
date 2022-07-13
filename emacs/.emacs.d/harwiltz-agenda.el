(require 'org-agenda)
(require 'find-lisp)
(message "Loading harwiltz's agenda config")

(setq org-clock-report-include-clocking-task t)
(setq org-agenda-start-with-clockreport-mode t)
(setq org-columns-default-format
      "%40ITEM(Task) %Effort(EE){:} %CLOCKSUM(Time Spent) %DEADLINE(Deadline)")
(setq harwiltz/org-agenda-base "/home/harwiltz/.org-agenda")
(setq org-agenda-files
      (find-lisp-find-files
       harwiltz/org-agenda-base "\.org$"))
(setq harwiltz/org-agenda-scheduled-marker-expr
      "(let ((scheduled (org-get-scheduled-time (point)))) (if scheduled \"S\" \" \"))")
(setq harwiltz/org-agenda-deadline-marker-expr
      "(let ((deadline (org-get-deadline-time (point)))) (if deadline \"D\" \" \"))")
(setq org-agenda-prefix-format
      `((agenda . " %i %-12:c%?-12t% s")
	(todo . ,(concat " %i %-12:c %"
			harwiltz/org-agenda-scheduled-marker-expr
			"%"
			harwiltz/org-agenda-deadline-marker-expr
			" %(symbol-name 'E)%-6 e"))
	;; (todo . " %i %-12:c %(symbol-name 'E)%-6 e")
	(tags . " %i %-12:c %(symbol-name 'E)%-6 e")
	(search . " %i %-12:c %(symbol-name 'E)%-6 e")))
(setq org-agenda-custom-commands
      '(("h" "Main agenda"
	 ((agenda "")
	  (todo "TODO"
		((org-agenda-overriding-header "To Refile")
		 (org-agenda-files
		  (list (concat harwiltz/org-agenda-base "/inbox.org")))))
	  (todo "IN PROGRESS"
		((org-agenda-overriding-header "In Progress")))
	  (todo "WAITING"
		((org-agenda-overriding-header "Waiting")))
	  (todo "TODO"
		((org-agenda-overriding-header "Backlog")
		 (org-agenda-files
		  (list (concat harwiltz/org-agenda-base "/backlog.org")))))
	  (todo "TODO"
		((org-agenda-overriding-header "Upcoming Events")
		 (org-agenda-files
		  (list (concat harwiltz/org-agenda-base "/events.org"))))))
		 ;; (org-agenda-skip-function
		 ;;  '(org-agenda-skip-entry-if 'deadline 'scheduled)))))
	 nil)))

(setq org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-refile-targets '(("backlog.org" :level . 1)
			   ("events.org" :level . 1)
			   ("test.org" :level . 1)))

(add-hook 'org-agenda-mode-hook #'hl-line-mode)
(add-hook 'org-after-refile-insert-hook 'org-save-all-org-buffers)

(defun scrum-clocktable-write (&rest args)
  "Custom clocktable formatter for placing the
Effort column next to the Time column."
  (apply #'org-clocktable-write-default args)
  (save-excursion
    (forward-char)                ;; [File] Effort Headline Time
    (org-table-next-field)        ;; File [Effort] Headline Time
    (org-table-move-column-right) ;; File Headline [Effort] Time
    ))

(setq org-agenda-clockreport-parameter-plist
      '(:link t :maxlevel 2 :narrow 80 :properties ("Effort")
	      :formatter scrum-clocktable-write))

(defun harwiltz/process-inbox-task ()
  (interactive)
  (org-with-wide-buffer
   (org-agenda-priority)
   (org-agenda-set-effort)
   (org-agenda-set-tags)
   (org-agenda-refile)))

(defun harwiltz/unprocess-task ()
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'org-agenda-deadline))
  (org-agenda-priority)
  (org-agenda-set-tags "processed" 'off))


(global-set-key (kbd "<f1>") (lambda () (interactive) (org-agenda nil "h")))


(setq org-capture-templates
      `(("i" "inbox" entry
	 (file ,(concat harwiltz/org-agenda-base "/inbox.org"))
	 "* TODO [%<%y%m%d%s>] %?")
	("t" "task for inbox" entry
	 (file ,(concat harwiltz/org-agenda-base "/inbox.org"))
	 "* TODO [%<%y%m%d%s>] %^{prompt}" :immediate-finish t)))

(global-set-key (kbd "C-c c") 'org-capture)
(define-key org-agenda-mode-map "R" 'harwiltz/process-inbox-task)
