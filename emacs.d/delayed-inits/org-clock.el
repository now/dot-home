(require 'now-org)

(setq org-clock-in-switch-to-state 'now-org-clock-in-switch-to-state
      org-clock-out-remove-zero-time-clocks t
      org-clock-out-when-done '("HOLD" "WAIT" "DLGT" "DONE" "NIXD")
      org-clock-persist t
      org-clock-persist-query-resume nil
      org-clock-report-include-clocking-task t)

(defun now-org-clock-in-switch-to-state (state)
  "Adjust task keyword based on STATE.  If we are in
`org-capture-mode', do nothing.  Otherwise, if STATE is TODO and
the current headline is not a `now-org-project-p', return NEXT."
  (cond
   ((and (boundp 'org-capture-mode) org-capture-mode))
   ((and (string= state "TODO")
         (not (now-org-has-descendant-p #'now-org-task-p))) "NEXT")))
