(setq org-agenda-clockreport-parameter-plist '(:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80)
      org-agenda-compact-blocks t
      org-agenda-custom-commands '((" " "Agenda"
                                    ((agenda    "")
                                     (tags      "REFILE"
                                                ((org-agenda-overriding-header "Refilables")
                                                 (org-tags-match-list-sublevels nil)))
                                     (tags-todo "-NIXD"
                                                ((org-agenda-overriding-header "Stuck Projects")
                                                 (org-agenda-skip-function 'now-org-skip-unless-stuck-project)))
                                     (tags-todo "-HOLD-NIXD"
                                                ((org-agenda-overriding-header "Projects")
                                                 (org-agenda-skip-function 'now-org-skip-unless-active-project)
                                                 (org-tags-match-list-sublevels 'indented)))
                                     (tags-todo "-REFILE-HOLD-WAIT-NIXD/!NEXT"
                                                ((org-agenda-overriding-header "Project Next Tasks")
                                                 (org-agenda-skip-function 'now-org-skip-unless-project-task)
                                                 (org-agenda-todo-ignore-scheduled t)
                                                 (org-agenda-todo-ignore-deadlines t)
                                                 (org-agenda-todo-ignore-with-date t)))
                                     (tags-todo "-REFILE-HOLD-WAIT-NIXD/!-NEXT"
                                                ((org-agenda-overriding-header "Project Tasks")
                                                 (org-agenda-skip-function 'now-org-skip-unless-project-task)
                                                 (org-agenda-todo-ignore-scheduled t)
                                                 (org-agenda-todo-ignore-deadlines t)
                                                 (org-agenda-todo-ignore-with-date t)))
                                     (tags-todo "-REFILE-HOLD-WAIT-NIXD"
                                                ((org-agenda-overriding-header "Standalone Tasks")
                                                 (org-agenda-skip-function 'now-org-skip-unless-standalone-task)
                                                 (org-agenda-todo-ignore-scheduled t)
                                                 (org-agenda-todo-ignore-deadlines t)
                                                 (org-agenda-todo-ignore-with-date t)))
                                     (tags-todo "WAIT|HOLD-NIXD"
                                                ((org-agenda-overriding-header "Waiting and Held Tasks")
                                                 (org-agenda-skip-function 'now-org-skip-stuck-projects)
                                                 (org-tags-match-list-sublevels nil)
                                                 (org-agenda-todo-ignore-scheduled t)
                                                 (org-agenda-todo-ignore-deadlines t)))
                                     (tags      "-REFILE/DONE|NIXD"
                                                ((org-agenda-overriding-header "Archivables")
                                                 (org-tags-match-list-sublevels nil))))))
      org-agenda-deadline-leaders '("Deadline:  " "In %d days: " "%d days ago: ")
      org-agenda-diary-file (concat (file-name-as-directory org-directory) "diary.org")
      org-agenda-dim-blocked-tasks nil
      org-agenda-log-mode-items '(clocked closed state)
      org-agenda-prefix-format '((agenda . " %i %-13:c%?-12t% s")
                                 (timeline . "  % s")
                                 (todo . " %i %-13:c")
                                 (tags . " %i %-13:c")
                                 (search . " %i %-13:c"))
      org-agenda-span 'day
      org-agenda-use-time-grid nil)

(defun now-org-each-descendant (f)
  "Call F for each descendant of the current heading."
  (let ((level (funcall outline-level)))
    (save-excursion
      (while (and (progn
		    (outline-next-heading)
		    (> (funcall outline-level) level))
		  (not (eobp)))
	(funcall f)))))

(defun now-org-each-child (function)
  "Call FUNCTION for each child of the current heading."
  (save-excursion
    (when (org-goto-first-child)
      (funcall function)
      (while (org-get-next-sibling)
        (funcall function)))))

(defun now-org-find-tree (function)
  "Find the first occurrence of a FUNCTION returning a non-nil value."
  (catch 'exit
    (org-map-tree (lambda () (if (funcall function) (throw 'exit t))))))

(defun now-org-has-descendant-p (g)
  "TBD."
  (catch 'done
    (now-org-each-descendant (lambda ()
                               (if (funcall g)
                                   (throw 'done t))))))

(defun now-org-project-p ()
  "Return `t' if `point' is inside a project, that is, a headline
with a todo keyword and a sub-headline with a todo keyword.  This
function assumes that `point' is at a project task."
  (now-org-has-descendant-p (lambda () (org-get-todo-state))))

(defun now-org-stuck-project-p ()
  (and (not (now-org-active-project-p)) (now-org-project-p)))

(defun now-org-active-project-p ()
  "Return `t' if `point' is inside an active project, that is, a
project with a sub-task with a todo keyword set to NEXT that
doesn’t have a WAIT tag (inherited or direct).  This function
assumes that `point' is at a project task, that is, a headline
with a todo keyword."
  (now-org-has-descendant-p (lambda ()
                              (and (string= (org-get-todo-state) "NEXT")
                                   (not (member "WAIT" (org-get-tags-at)))))))

(defun now-org-skip-unless-stuck-project ()
  "Skip Org tasks that aren’t projects or that are projects that are active."
  (unless (now-org-stuck-project-p)
    (save-excursion (or (outline-next-heading) (point-max)))))

(defun now-org-skip-unless-active-project ()
  "Skip Org tasks that aren’t projects or that are projects that aren’t active."
  (unless (now-org-active-project-p)
    (save-excursion (or (outline-next-heading) (point-max)))))

(defun now-org-project-task-p ()
  "Return `t' if `point' is inside a project."
  (save-excursion
    (save-restriction
      (widen)
      (and (org-up-heading-safe) (org-get-todo-state)))))

(defun now-org-skip-unless-project-task ()
  "Skip tasks that are projects or that are standalone tasks."
  (if (or (now-org-project-p) (not (now-org-project-task-p)))
      (save-excursion (or (outline-next-heading) (point-max)))))

(defun now-org-skip-unless-standalone-task ()
  "Skip tasks that are projects."
  (if (or (now-org-project-p) (now-org-project-task-p))
      (save-excursion (org-end-of-subtree t))))

(defun now-org-skip-stuck-projects ()
  ""
  (if (now-org-stuck-project-p)
      (save-excursion (or (outline-next-heading) (point-max)))))
