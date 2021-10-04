(require 'now-org-agenda)

;;;###autoload
(defun now-org-clock-in-switch-to-state (state)
  "Adjust task keyword based on STATE.  If we are in
`org-capture-mode', do nothing.  Otherwise, if STATE is TODO and
the current headline is not a `now-org-project-p', return NEXT."
  (cond
   ((and (boundp 'org-capture-mode) org-capture-mode))
   ((and (string= state "TODO")
         (not (now-org-has-descendant-p #'now-org-task-p))) "NEXT")))

(provide 'now-org-clock)
