(defun now-org-refile-target-verify ()
  "Verify that refile target is not a done task."
  (not (org-entry-is-done-p)))
(defface org-delegated nil
  "Face used for DELEGATED todo keyword."
  :group 'org-faces)
(defface org-hold nil
  "Face used for HOLD todo keyword."
  :group 'org-faces)
(defface org-next nil
  "Face used for NEXT todo keyword."
  :group 'org-faces)
(defface org-waiting nil
  "Face used for WAIT todo keyword."
  :group 'org-faces)
(setq org-directory "~/Documents/Org"
      org-agenda-deadline-faces '((0.0 . default))
      org-agenda-files (list org-directory)
      org-agenda-text-search-extra-files '(argenda-archives)
      org-catch-invisible-edits 'smart
      org-columns-default-format "%80ITEM(Task) %7Effort{:} %7CLOCKSUM(Clocked)"
      org-columns-ellipses "…"
      org-default-notes-file (concat (file-name-as-directory org-directory)
                                     "refile.org")
      org-edit-src-persistent-message nil
      org-enforce-todo-dependencies t
      org-global-properties '(("Effort_ALL" .
                               "0:15 0:30 1:00 2:00 4:00 8:00 0:00"))
      org-hide-emphasis-markers t
      org-highlight-sparse-tree-matches nil
      org-link-frame-setup '((vm . vm-visit-folder)
                             (gnus . org-gnus-no-new-news)
                             (file . find-file)
                             (wl . wl))
      org-log-done 'time
      org-log-into-drawer t
      org-log-redeadline t
      org-log-reschedule t
      org-loop-over-headlines-in-active-region 'region-start-level
      org-reverse-note-order t
      org-outline-path-complete-in-steps nil
      org-refile-allow-creating-parent-nodes 'confirm
      org-refile-target-verify-function 'now-org-refile-target-verify
      org-refile-targets '((org-agenda-files . (:maxlevel . 9)))
      org-refile-use-outline-path 'file
      org-src-window-setup 'current-window
      org-todo-keyword-faces '(("DLGT" . org-delegated)
                               ("HOLD" . org-hold)
                               ("NEXT" . org-next)
                               ("WAIT" . org-waiting))
      org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                          (sequence "HOLD(h)" "WAIT(w@/@)" "DLGT(g@/!)" "|"
                                    "NIXD(c@/!)")
                          (type "CALL")
                          (type "CHAT"))
      org-treat-S-cursor-todo-selection-as-state-change nil
      org-yank-adjusted-subtrees t)
(setf (cdr (assq 'state org-log-note-headings)) "State %s from %S %t")

(add-to-list 'org-file-apps '(directory . emacs))

(defun now-org-insert-heading-finish-log-note ()
  (remove-hook 'org-log-buffer-setup-hook
               'now-org-insert-heading-finish-log-note)
  (org-ctrl-c-ctrl-c))

(defun now-org-insert-heading-add-log-note ()
  "Insert a logbook note under the current headline."
  (add-hook 'org-log-buffer-setup-hook
            'now-org-insert-heading-finish-log-note
            'append)
  (org-add-log-setup 'note nil nil 'findpos ""))

(add-hook 'org-insert-heading-hook 'now-org-insert-heading-add-log-note)

(defun now-org-set-delegatee-property ()
  "Set the Delegatee property when a task is marked as DLGT."
  (remove-hook 'post-command-hook 'now-org-set-delegatee-property)
  (if (marker-position org-log-note-return-to)
      (with-current-buffer (marker-buffer org-log-note-return-to)
        (save-excursion
          (goto-char org-log-note-return-to)
          (org-back-to-heading t)
          (org-set-property "Delegatee" nil)))
    (org-set-property "Delegatee" nil)))

(defvar org-last-state)
(defvar org-state)
(defun now-org-adjust-properties-after-todo-state-change ()
  "Adjust properties after a task todo state change."
  (unless (string= org-last-state org-state)
    (cond ((string= org-state "DLGT")
           (add-hook 'post-command-hook 'now-org-set-delegatee-property 'append))
          ((and (string= org-last-state "DLGT") org-last-todo-state-is-todo)
           (org-delete-property "Delegatee")))))

(add-hook 'org-after-todo-state-change-hook
          'now-org-adjust-properties-after-todo-state-change)

(defun now-org-sort-entries ()
  "Sort entries by TODO state, priority, effort, and timestamp."
  (format "%03d %03d %02d %03d %+026.6f"
          (if (looking-at org-complex-heading-regexp)
              (let ((m (match-string 2)))
                (if m
                    (- 99 (length (member m org-todo-keywords-1)))
                  0))
            0)
          (if (looking-at org-complex-heading-regexp)
              (let* ((m (match-string 2))
                     (s (if (member m org-done-keywords) '- '+)))
                (if m
                    (funcall s (length (member m org-todo-keywords-1)))
                  0))
            0)
          (if (save-excursion (re-search-forward org-priority-regexp (point-at-eol) t))
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

(defun now-org-sort (with-case)
  "Like `org-sort', except use `now-org-sort-entries' directly."
  (interactive "P")
  (cond
   ((org-at-table-p) (org-call-with-arg 'org-table-sort-lines with-case))
   ((org-at-item-p) (org-call-with-arg 'org-sort-list with-case))
   (t (org-sort-entries with-case ?f 'now-org-sort-entries)
      (outline-hide-subtree)
      (org-cycle))))

(defun now-org-narrow-to-subtree-and-show-todo-tree ()
  "Narrow buffer to TODO entries in the current subtree."
  (interactive)
  (widen)
  (while (org-up-heading-safe))
  (org-narrow-to-subtree)
  (org-show-todo-tree '(4)))

(if (boundp 'narrow-map)
    (org-defkey narrow-map "T" 'now-org-narrow-to-subtree-and-show-todo-tree)
  (org-defkey org-mode-map "\C-xnT"
              'now-org-narrow-to-subtree-and-show-todo-tree))

(org-clock-persistence-insinuate)
