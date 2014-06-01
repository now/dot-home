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
                                     (tags-todo "-REFILE-WAIT-HOLD-NIXD/!NEXT"
                                                ((org-agenda-overriding-header "Next Tasks")
                                                 (org-agenda-skip-function 'now-org-skip-unless-next-tasks)
                                                 (org-agenda-todo-ignore-scheduled t)
                                                 (org-agenda-todo-ignore-deadlines t)
                                                 (org-agenda-todo-ignore-with-date t)))
                                     (tags-todo "-REFILE-WAIT-HOLD-NIXD/!"
                                                ((org-agenda-overriding-header "Standalone Tasks")
                                                 (org-agenda-skip-function 'now-org-skip-unless-tasks)
                                                 (org-agenda-todo-ignore-scheduled t)
                                                 (org-agenda-todo-ignore-deadlines t)
                                                 (org-agenda-todo-ignore-with-date t)))
                                     (tags      "-REFILE/DONE|NIXD"
                                                ((org-agenda-overriding-header "Archivables")
                                                 (org-tags-match-list-sublevels nil))))
                                    nil))
      org-agenda-deadline-leaders '("Deadline:  " "In %d days: " "%d days ago: ")
      org-agenda-diary-file (concat (file-name-as-directory org-directory) "diary.org")
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

(defun now-org-find-tree (function)
  "Find the first occurrence of a FUNCTION returning a non-nil value."
  (catch 'exit
    (org-map-tree (lambda () (if (funcall function) (throw 'exit t))))))

(defun now-org-project-p ()
  "Return `t' if `point' is inside a project, that is, a headline
with a todo keyword and a sub-headline with a todo keyword.  This
function assumes that `point' is at a project task."
  (catch 'exit
    (now-org-each-descendant (lambda ()
                               (if (org-get-todo-state)
                                   (throw 'exit t))))))

(defun now-org-active-project-p ()
  "Return `t' if `point' is inside an active project, that is, a
project with a sub-task with a todo keyword set to NEXT that
doesn’t have a WAIT tag (inherited or direct).  This function
assumes that `point' is at a project task, that is, a headline
with a todo keyword."
  (catch 'exit
    (now-org-each-descendant (lambda ()
                               (if (and (string= (org-get-todo-state) "NEXT")
                                        (not (member "WAIT" (org-get-tags-at))))
                                   (throw 'exit t))))))

(defun now-org-skip-unless-stuck-project ()
  "Skip Org tasks that aren’t projects or that are projects that are active."
  (if (or (now-org-active-project-p) (not (now-org-project-p)))
      (save-excursion (or (outline-next-heading) (point-max)))))

(defun now-org-skip-unless-active-project ()
  "Skip Org tasks that aren’t projects or that are projects that aren’t active."
  (unless (now-org-active-project-p)
    (save-excursion (or (outline-next-heading) (point-max)))))

(defun now-org-project-task-p ()
  "Return `t' if `point' is inside a project."
  (save-excursion
    (and (org-up-heading-safe) (org-get-todo-state))))

(defun now-org-skip-unless-next-tasks ()
  "Skip tasks that are projects or that are standalone tasks."
  (if (or (now-org-project-p) (not (now-org-project-task-p)))
      (save-excursion (or (outline-next-heading) (point-max)))))

(defun now-org-skip-unless-tasks ()
  "Skip tasks that are projects."
  (if (or (now-org-project-p) (now-org-project-task-p))
      (save-excursion (org-end-of-subtree t))))
