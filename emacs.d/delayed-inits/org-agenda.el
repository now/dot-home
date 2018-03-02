(require 'now-org)

(setq org-agenda-clockreport-parameter-plist '(:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80)
      org-agenda-compact-blocks t
      org-agenda-custom-commands `(("A" "Archivals"
                                    ((tags "-REFILE/DONE|NIXD"
                                           ((org-agenda-overriding-header "Archivals")
                                            (org-agenda-skip-function 'now-org-agenda-skip-unless-archival)
                                            (org-tags-match-list-sublevels nil))))))
      org-agenda-deadline-leaders '("Deadline:  " "In %d days: " "%d days ago: ")
      org-agenda-diary-file (concat (file-name-as-directory org-directory) "personal.org")
      org-agenda-dim-blocked-tasks nil
      org-agenda-fontify-priorities t
      org-agenda-insert-diary-strategy 'date-tree-last
      org-agenda-tags-todo-honor-ignore-options t
      org-agenda-log-mode-items '(clocked closed state)
      org-agenda-prefix-format '((agenda . "% i%-11c%?-12t% s")
                                 (timeline . "% s")
                                 (todo . "% i%-11c")
                                 (tags . "% i%-11c")
                                 (search . "% i%-11c"))
      org-agenda-sorting-strategy '((agenda habit-down time-up priority-down effort-up timestamp-down category-keep)
                                    (todo priority-down effort-up timestamp-down category-up)
                                    (tags priority-down effort-up timestamp-down category-up)
                                    (search category-up))
      org-agenda-sticky t
      org-agenda-time-leading-zero t
      org-agenda-use-time-grid nil)

(declare-function org-clock-special-range "org-clock.el")
(defvar org-clock-file-total-minutes)
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
                                     (cadr  (org-clock-special-range 'thismonth)))
                      org-clock-file-total-minutes))
                  0)))
      (save-excursion (org-end-of-subtree t))))

(define-key org-agenda-mode-map "`" 'smex)
(define-key org-agenda-mode-map "\M-d" 'smex)
(define-key org-agenda-mode-map "n" 'org-agenda-next-item)
(define-key org-agenda-mode-map "p" 'org-agenda-previous-item)

(add-hook 'org-agenda-mode-hook 'hl-line-mode)
