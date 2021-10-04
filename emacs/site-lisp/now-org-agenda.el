(require 'org)
(require 'org-agenda)
(require 'org-clock)
(require 'org-element)

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

(defun now-org-has-descendant-p (g)
  "Return t if G returns t for any descendant of the current heading."
  (catch 'done
    (now-org-each-descendant (lambda () (if (funcall g) (throw 'done t))))))

(defalias 'now-org-task-p 'org-get-todo-state
  "Return t if `point' is on a task headline.  A task headline
  is a headline with a todo keyword.")

(defun now-org-project-p ()
  "Return t if `point' is on a project headline.  A project
headline is a headline with `org-get-todo-state' TODO or a
`now-org-task-p' headline that has a `now-org-task-p'
sub-headline.

The reasoning for the first branch is that it’s not known whether
a TODO headline is a project or not, which means that there’s at
least the meta-project of determining whether it is a project or
not to complete.  A task known not to be a project should be in
state NEXT.

This function assumes that it’s being called on a
`now-org-task-p' headline."
  (or (string= (org-get-todo-state) "TODO")
      (now-org-has-descendant-p #'now-org-task-p)))

(defun now-org-project-task-p ()
  "Return t if `point' is on a project task headline.  A project
task headline is a `now-org-task-p' sub-headline of a
`now-org-task-p' headline."
  (save-excursion
    (save-restriction
      (widen)
      (and (org-up-heading-safe) (now-org-task-p)))))

;;;###autoload
(defun now-org-agenda-skip-unless-archival ()
  "Skip tasks that are not ready for archival.  A task is ready
for archival if it is not a `now-org-project-task-p' or if it is
not a `now-org-project-p' that has time clocked on it during this
or the previous month."
  (if (or (now-org-project-task-p)
          (and (now-org-project-p)
               (> (save-excursion
                    (save-restriction
                      (org-narrow-to-subtree)
                      (org-clock-sum (car (org-clock-special-range 'lastmonth))
                                     (cadr (org-clock-special-range 'thismonth)))
                      org-clock-file-total-minutes))
                  0)))
      (save-excursion (org-end-of-subtree t))))

(provide 'now-org-agenda)
