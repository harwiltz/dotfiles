(setq org-agenda-files
      (mapcar (lambda (x) (concat harwiltz/agenda-dir "/" x))
	      '("tasks.org")))

(setq org-priority-default 68)

(setq org-agenda-custom-commands
      '(("d" "Dashboard"
	 ((agenda "" ((org-deadline-warning-days 7)))
	  (tags "active" ((org-agenda-overriding-header "Active Tasks")))
	  (todo "NEXT" ((org-agenda-overriding-header "Bullpen")))
	  (todo "BACKLOG" ((org-agenda-overriding-header "Backlog")))
	  (tags-todo "+PRIORITY=\"D\"" ((org-agenda-overriding-header "Unclassified")))))))

(push `("t" "Task" entry (file ,(concat harwiltz/agenda-dir "/tasks.org"))
	"* BACKLOG %?"
	:kill-buffer t)
      org-capture-templates)
