(require 'org)

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

;; TODO Add optional known-to-be-task-p parameter.
(defun now-org-project-p ()
  "Return t if `point' is on a project headline.  A project
headline is a `now-org-task-p' headline that has a
`now-org-task-p' sub-headline."
  (now-org-has-descendant-p #'now-org-task-p))

(defun now-org-active-project-p ()
  "Return t if `point' is on an active project headline.  An
active project headline is a `now-org-task-p' headline that has a
sub-headline with a todo keyword set to NEXT that does not have a
WAIT tag (inherited or direct)."
  (now-org-has-descendant-p (lambda ()
                              (and (string= (org-get-todo-state) "NEXT")
                                   (not (member "WAIT" (org-get-tags-at)))))))

(defun now-org-stuck-project-p ()
  "Return t if `point' is on a stuck project headline.  A stuck
project headline is a `now-org-project-p' headline that is not a
`now-org-active-project-p' headline."
  (and (not (now-org-active-project-p)) (now-org-project-p)))

(defun now-org-project-task-p ()
  "Return t if `point' is on a project task headline.  A project
task headline is a `now-org-task-p' sub-headline of a
`now-org-task-p' headline."
  (save-excursion
    (save-restriction
      (widen)
      (and (org-up-heading-safe) (now-org-task-p)))))

(defun now-org-standalone-task-p ()
  "Return t if `point' is on a standalone task headline.  A
standalone task headline is a `now-org-task-p' headline that is
not a `now-org-project-p' headline nor a `now-org-project-task-p'
headline."
  (not (or (now-org-project-p) (now-org-project-task-p))))

(provide 'now-org)
