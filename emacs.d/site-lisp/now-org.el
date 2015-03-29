(require 'org)
(require 'org-agenda)
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

(defun now-org-active-project-p ()
  "Return t if `point' is on an active project headline.  An
active project headline is a `now-org-task-p' headline that has a
sub-headline with a todo keyword set to NEXT that does not have a
WAIT tag (inherited or direct).

This function assumes that it’s being called on a
`now-org-project-p' headline."
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

(defun now-org-cmp-projects (a b)
  "Compare projects A and B.  Calls `now-org-cmp-projects-at' for
the 'org-marker of each."
  (now-org-cmp-projects-at (get-text-property 1 'org-marker a)
                           (get-text-property 1 'org-marker b)))

(defun now-org-cmp-projects-at (a b)
  "Compare project headline A with project headline B.  This is
done by first comparing their `org-outline-level'.  If either is
smaller, compare recursively against the parent, if possible.  If
not possible, either -1 or +1 will be returned.  If the outline
level is the same, compare priorities, highest first.  If
priorities are the same, compare their
`now-org-most-recent-inactive-timestamp-in-tree'."
  (let ((la (org-with-point-at a (org-outline-level)))
        (lb (org-with-point-at b (org-outline-level))))
    (cond
     ((< la lb)
      (let* ((b-up (org-with-point-at b
                     (while (and (org-up-heading-safe)
                                 (< la (org-outline-level))))
                     ;; We know that this is a project, as the child is a task.
                     (when (and (>= la (org-outline-level)) (now-org-task-p))
                       (point-marker))))
             (result (if (and b-up (not (= a b-up)))
                         (now-org-cmp-projects-at a b-up)
                       -1)))
        (when b-up
          (set-marker b-up nil))
        result))
     ((> la lb)
      (let ((r (now-org-cmp-projects-at b a)))
        (when r (- r))))
     (t
      (let ((pa (now-org-get-priority-at a))
            (pb (now-org-get-priority-at b)))
        (cond
         ((< pa pb) +1)
         ((> pa pb) -1)
         (t
          (let ((ta (now-org-most-recent-inactive-timestamp-in-tree a))
                (tb (now-org-most-recent-inactive-timestamp-in-tree b)))
            (cond
             ((and ta tb)
              (cond
               ((time-less-p ta tb) +1)
               ((time-less-p tb ta) -1)))
             (ta -1)
             (tb +1))))))))))

(defun now-org-get-priority-at (pom)
  "Return priority at POM. Return -1000 if not at a headline or
no priority has been set."
  (org-with-point-at pom
    (beginning-of-line)
    (save-match-data
      (when (looking-at org-heading-regexp)
        (or (org-get-priority (match-string 0)) -1000)))))

(defun now-org-most-recent-inactive-timestamp-in-tree (pom)
  "Return the most recent inactive timestamp in tree under POM."
  (org-with-point-at pom
    (let ((end (save-excursion (org-end-of-subtree t)))
          (case-fold-search nil)
          ts)
      (while (re-search-forward (org-re-timestamp 'inactive) end t)
        (backward-char)
        (let ((object (org-element-context)))
          (when (memq (org-element-type object)
                      '(node-property timestamp))
            (let ((type (org-element-property :type object)))
              (when  (memq type '(inactive inactive-range))
                (let ((tst (ignore-errors
                            (org-time-string-to-time (org-element-property
                                                      :raw-value object)))))
                  (when (and tst (or (not ts) (time-less-p ts tst)))
                    (setq ts tst))))))))
      ts)))

(defun now-org-agenda-next-time-span (&optional arg)
  "Switch to next time span view for agenda.
With ARG, switch to that item in the next time span.

The cycle of time spans is

  day         week
  week        month
  fortnight   month
  month       year
  year        day"
  (interactive "P")
  (let* ((args (get-text-property (min (1- (point-max)) (point)) 'org-last-args))
	 (curspan (nth 2 args))
         (nextspan (or (cdr (assoc curspan '((day . week)
                                             (week . month)
                                             (fortnight . month)
                                             (month . year)
                                             (year . day))))
                       'day)))
    (org-agenda-change-time-span nextspan arg)))

(provide 'now-org)
