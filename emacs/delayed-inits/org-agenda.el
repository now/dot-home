(setq org-agenda-clockreport-parameter-plist '(:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80)
      org-agenda-compact-blocks t
      org-agenda-custom-commands '((" " "Agenda"
                                    ((agenda    "")
                                     (tags      "REFILE"
                                                ((org-agenda-overriding-header "Refilables")
                                                 (org-tags-match-list-sublevels nil)))
                                     (tags      "-NIXD"
                                                ((org-agenda-overriding-header "Stuck Projects")
                                                 (org-agenda-skip-function 'now-org-skip-non-projects-and-active-projects)))
                                     (tags      "-HOLD-NIXD"
                                                ((org-agenda-overriding-header "Projects")
                                                 (org-agenda-skip-function 'now-org-skip-non-projects-and-stuck-projects)
                                                 (org-tags-match-list-sublevels 'indented)))
                                     (tags-todo "-REFILE-WAIT-HOLD-NIXD/!NEXT"
                                                ((org-agenda-overriding-header "Nexts")
                                                 (org-agenda-skip-function 'now-org-skip-projects-and-standalone-tasks)
                                                 (org-agenda-todo-ignore-scheduled t)
                                                 (org-agenda-todo-ignore-deadlines t)
                                                 (org-agenda-todo-ignore-with-date t)))
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

(defun now-org-each-descendant (function)
  "Call FUNCTION for each descendant of the current heading."
  (let ((level (funcall outline-level)))
    (save-excursion
      (while (and (progn
		    (outline-next-heading)
		    (> (funcall outline-level) level))
		  (not (eobp)))
	(funcall function)))))

(defun now-org-each-child (function)
  "Call FUNCTION for each child of the current heading."
  (save-excursion
    (when (org-goto-first-child)
      (funcall function)
      (while (org-get-next-sibling)
        (funcall function)))))

(defun now-org-project-p ()
  "Return `t' if `point' is inside a project, that is, a task
with a subtask with a todo keyword."
  (catch 'exit
    (now-org-each-child (lambda ()
                          (if (member (org-get-todo-state) org-todo-keywords-1)
                              (throw 'exit t))))))

(defun now-org-active-project-p (&optional known-to-be-project)
  "Return `t' if `point' is inside an active project, that is, a
project with a subtask with a todo keyword set to NEXT that
doesn’t have a WAIT tag (inherited or otherwise)."
  (if (or known-to-be-project (now-org-project-p))
      (catch 'exit
        (now-org-each-child (lambda ()
                              (if (and (string= (org-get-todo-state) "NEXT")
                                       (not (member "WAIT" (org-get-tags-at))))
                                  (throw 'exit t)))))))

(defun now-org-skip-non-projects-and-active-projects ()
  "Skip Org tasks that aren’t projects or that are projects that are active."
  (if (or (not (now-org-project-p)) (now-org-active-project-p t))
      (save-excursion (or (outline-next-heading) (point-max)))))

(defun now-org-skip-non-projects-and-stuck-projects ()
  "Skip Org tasks that aren’t projects or that are projects that aren’t active."
  (unless (and (now-org-project-p) (now-org-active-project-p t))
    (save-excursion (or (outline-next-heading) (point-max)))))

(defun now-org-project-task-p ()
  "Return `t' if `point' is inside a project."
  (catch 'exit
    (while (org-up-heading-safe)
      (if (now-org-project-p)
          (throw 'exit t)))))

(defun now-org-skip-projects-and-standalone-tasks ()
  "Skip tasks that are projects or that are standalone tasks."
  (message "testing %s" (nth 4 (org-heading-components)))
  (if (or (now-org-project-p) (not (now-org-project-task-p)))
      (save-excursion (or (outline-next-heading) (point-max)))))
