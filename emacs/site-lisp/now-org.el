(require 'org)

;;;###autoload
(defun now-org-refile-target-verify ()
  "Verify that refile target is not a done task."
  (cond
   ((looking-at (concat "^.*:" org-archive-tag ":.*$"))
    (org-end-of-subtree t)
    nil)
   ((org-entry-is-done-p) nil)
   (t t)))

;;;###autoload
(defun now-org-insert-heading-add-log-note ()
  "Insert a logbook note under the current headline."
  (add-hook 'org-log-buffer-setup-hook
            'now-org--insert-heading-finish-log-note
            'append)
  (org-add-log-setup 'note nil nil 'findpos ""))

(defun now-org--insert-heading-finish-log-note ()
  (remove-hook 'org-log-buffer-setup-hook
               'now-org-insert-heading-finish-log-note)
  (org-ctrl-c-ctrl-c))

(defvar org-last-state)
(defvar org-state)
;;;###autoload
(defun now-org-adjust-properties-after-todo-state-change ()
  "Adjust properties after a task todo state change."
  (unless (string= org-last-state org-state)
    (cond ((string= org-state "DLGT")
           (add-hook 'post-command-hook
                     'now-org--set-delegatee-property
                     'append))
          ((and (string= org-last-state "DLGT") org-last-todo-state-is-todo)
           (org-delete-property "Delegatee")))))

(defun now-org--set-delegatee-property ()
  "Set the Delegatee property when a task is marked as DLGT."
  (remove-hook 'post-command-hook 'now-org--set-delegatee-property)
  (if (marker-position org-log-note-return-to)
      (with-current-buffer (marker-buffer org-log-note-return-to)
        (save-excursion
          (goto-char org-log-note-return-to)
          (org-back-to-heading t)
          (org-set-property "Delegatee" nil)))
    (org-set-property "Delegatee" nil)))

;;;###autoload
(defun now-org-sort (with-case)
  "Like `org-sort', except use `now-org-sort-entries' directly."
  (interactive "P")
  (cond
   ((org-at-table-p) (org-call-with-arg 'org-table-sort-lines with-case))
   ((org-at-item-p) (org-call-with-arg 'org-sort-list with-case))
   (t (org-sort-entries with-case ?f 'now-org-sort-entries)
      (outline-hide-subtree)
      (org-cycle))))

;;;###autoload
(defun now-org-sort-entries ()
  "Sort entries by TODO state, priority, effort, and timestamp."
  (format "%03d %03d %02d %03d %+026.6f"
          (if (looking-at org-complex-heading-regexp)
              (let ((m (match-string 2))
                    (tags (match-string 5)))
                (cond
                 ((string-equal tags ":ARCHIVE:") 1000)
                 (m (- 99 (length (member m org-todo-keywords-1))))
                 (t 0)))
            0)
          (if (looking-at org-complex-heading-regexp)
              (let* ((m (match-string 2))
                     (s (if (member m org-done-keywords) '- '+)))
                (if m
                    (funcall s (length (member m org-todo-keywords-1)))
                  0))
            0)
          (if (save-excursion (re-search-forward org-priority-regexp
                                                 (point-at-eol) t))
              (string-to-char (match-string 2))
            org-default-priority)
          (org-duration-to-minutes
           (or (org-entry-get nil org-effort-property)
               (car (last (org-property-get-allowed-values
                           (point-min) org-effort-property)))))
          (let ((end (save-excursion (outline-next-heading) (point)))
                (now (float-time)))
            (- (if (or (re-search-forward org-ts-regexp end t)
                       (re-search-forward org-ts-regexp-both end t))
                   (org-time-string-to-seconds (match-string 0))
                 (float-time))
               now))))

;;;###autoload
(defun now-org-narrow-to-subtree-and-show-todo-tree ()
  "Narrow buffer to TODO entries in the current subtree."
  (interactive)
  (widen)
  (while (org-up-heading-safe))
  (org-narrow-to-subtree)
  (org-show-todo-tree '(4)))

(provide 'now-org)
