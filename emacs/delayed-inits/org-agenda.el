(setq org-agenda-clockreport-parameter-plist '(:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80)
      org-agenda-compact-blocks t
      org-agenda-custom-commands '((" " "Agenda"
                                    ((agenda    "")
                                     (tags      "REFILE"
                                                ((org-agenda-overriding-header "Refilables")
                                                 (org-tags-match-list-sublevels nil)))
                                     (tags "-NIXD"
                                                ((org-agenda-overriding-header "Stuck Projects")
                                                 (org-agenda-skip-function 'now-org-skip-active-projects)))
                                     (tags      "-REFILE/DONE|NIXD"
                                                ((org-agenda-overriding-header "Archivables")
                                                 (org-tags-match-list-sublevels nil))))
                                    nil))
      org-agenda-deadline-leaders '("Deadline:  " "In %d days: " "%d days ago: ")
      org-agenda-log-mode-items '(clocked closed state)
      org-agenda-prefix-format '((agenda . " %i %-13:c%?-12t% s")
                                 (timeline . "  % s")
                                 (todo . " %i %-13:c")
                                 (tags . " %i %-13:c")
                                 (search . " %i %-13:c"))
      org-agenda-span 'day
      org-agenda-use-time-grid nil)

(defun now-org-map-subtree (f)
  "Call F for every heading underneath the current one."
  (let ((level (funcall outline-level)))
    (save-excursion
      (while (and (progn
		    (outline-next-heading)
		    (> (funcall outline-level) level))
		  (not (eobp)))
	(funcall f)))))

(defun now-org-project-p ()
  "Return `t' if `point' is inside a project, that is, a task
with a subtask with a todo keyword."
  (catch 'exit
    (now-org-map-subtree (lambda ()
                           (if (member (org-get-todo-state) org-todo-keywords-1)
                               (throw 'exit t))))))

(defun now-org-active-project-p (&optional known-to-be-project)
  "Return `t' if `point' is inside an active project, that is, a
project with a subtask with a todo keyword set to NEXT that
doesn’t have a WAIT tag (inherited or otherwise)."
  (if (or known-to-be-project (now-org-project-p))
      (catch 'exit
        (now-org-map-subtree (lambda ()
                               (if (and (string= (org-get-todo-state) "NEXT")
                                        (not (member "WAIT" (org-get-tags-at))))
                                   (throw 'exit t)))))))

(defun now-org-skip-active-projects ()
  "Skip Org tasks that are active projects."
  (if (or (not (now-org-project-p)) (now-org-active-project-p t))
      (save-excursion (org-end-of-subtree t))))
