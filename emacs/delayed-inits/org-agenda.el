(require 'now-org)

(let* ((commands '((agenda    "")
                   (tags      "+REFILE"
                              ((org-agenda-overriding-header "Refilables")
                               (org-tags-match-list-sublevels nil)))
                   (tags-todo "-DLGT-NIXD"
                              ((org-agenda-overriding-header "Stuck Projects")
                               (org-agenda-skip-function 'now-org-agenda-skip-unless-stuck-project)))
                   (tags-todo "-HOLD-DLGT-NIXD"
                              ((org-agenda-overriding-header "Projects")
                               (org-agenda-skip-function 'now-org-agenda-skip-unless-active-project)
                               (org-tags-match-list-sublevels 'indented)))
                   (tags-todo "-REFILE-HOLD-DLGT-WAIT-NIXD/!NEXT"
                              ((org-agenda-overriding-header "Project Next Tasks")
                               (org-agenda-skip-function 'now-org-agenda-skip-unless-project-task)
                               (org-agenda-todo-ignore-scheduled t)
                               (org-agenda-todo-ignore-deadlines t)
                               (org-agenda-todo-ignore-with-date t)))
                   (tags-todo "-REFILE-HOLD-DLGT-WAIT-NIXD/!-NEXT"
                              ((org-agenda-overriding-header "Project Tasks")
                               (org-agenda-skip-function 'now-org-agenda-skip-unless-project-task)
                               (org-agenda-todo-ignore-scheduled t)
                               (org-agenda-todo-ignore-deadlines t)
                               (org-agenda-todo-ignore-with-date t)))
                   (tags-todo "-REFILE-HOLD-DLGT-WAIT-NIXD"
                              ((org-agenda-overriding-header "Standalone Tasks")
                               (org-agenda-skip-function 'now-org-agenda-skip-unless-standalone-task)
                               (org-agenda-todo-ignore-scheduled t)
                               (org-agenda-todo-ignore-deadlines t)
                               (org-agenda-todo-ignore-with-date t)))
                   (tags-todo "+WAIT-DLGT-NIXD|+HOLD-DLGT-NIXD"
                              ((org-agenda-overriding-header "Waiting and Held Tasks")
                               (org-agenda-skip-function 'now-org-agenda-skip-stuck-projects)
                               (org-tags-match-list-sublevels nil)
                               (org-agenda-todo-ignore-scheduled t)
                               (org-agenda-todo-ignore-deadlines t)))
                   (tags-todo "+DLGT"
                              ((org-agenda-overriding-header "Delegated Tasks")
                               (org-agenda-todo-match-list-sublevels nil)
                               (org-agenda-todo-ignore-scheduled t)
                               (org-agenda-todo-ignore-deadlines t)))
                   (tags      "-REFILE/DONE|NIXD"
                              ((org-agenda-overriding-header "Archivals")
                               (org-agenda-skip-function 'now-org-skip-unless-archival)
                               (org-tags-match-list-sublevels nil)))))
       (add-tag (lambda (tag s)
                  (let ((parts (split-string s "/")))
                    (mapconcat 'identity
                               (cons (mapconcat (lambda (f) (concat tag f))
                                                (split-string (car parts) "|")
                                                "|")
                                     (cdr parts))
                               "/"))))
       (tag-commands (lambda (tag)
                       (mapcar (lambda (e)
                                 (if (eq 'tags-todo (car e))
                                     (cons (car e) (cons (funcall add-tag tag (cadr e)) (cddr e)))
                                   e))
                               commands))))
  (setq org-agenda-clockreport-parameter-plist '(:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80)
        org-agenda-compact-blocks t
        org-agenda-custom-commands `((" " "Agenda"
                                      ,commands)
                                     ("p" "Personal Agenda"
                                      ,(funcall tag-commands "personal"))
                                     ("w" "Work Agenda"
                                      ,(funcall tag-commands "work")))
        org-agenda-deadline-leaders '("Deadline:  " "In %d days: " "%d days ago: ")
        org-agenda-diary-file (concat (file-name-as-directory org-directory) "diary.org")
        org-agenda-dim-blocked-tasks nil
        org-agenda-log-mode-items '(clocked closed state)
        org-agenda-prefix-format '((agenda . " %i %-13:c%?-12t% s")
                                   (timeline . "  % s")
                                   (todo . " %i %-13:c")
                                   (tags . " %i %-13:c")
                                   (search . " %i %-13:c"))
        org-agenda-sorting-strategy '((agenda habit-down time-up effort-up priority-down category-keep)
                                      (todo category-up priority-down effort-up)
                                      (tags category-up priority-down effort-up)
                                      (search category-up))
        org-agenda-span 'day
        org-agenda-use-time-grid nil))

(defun now-org-agenda-skip-unless-stuck-project ()
  "Skip Org tasks that aren’t projects or that are projects that are active."
  (unless (now-org-stuck-project-p)
    (save-excursion (or (outline-next-heading) (point-max)))))

(defun now-org-agenda-skip-unless-active-project ()
  "Skip Org tasks that aren’t projects or that are projects that aren’t active."
  (unless (now-org-active-project-p)
    (save-excursion (or (outline-next-heading) (point-max)))))

(defun now-org-agenda-skip-unless-project-task ()
  "Skip tasks that are projects or that are standalone tasks."
  (if (or (now-org-project-p) (not (now-org-project-task-p)))
      (save-excursion (or (outline-next-heading) (point-max)))))

(defun now-org-agenda-skip-unless-standalone-task ()
  "Skip tasks that are projects."
  (unless (now-org-standalone-task-p)
    (save-excursion (org-end-of-subtree t))))

(defun now-org-agenda-skip-stuck-projects ()
  ""
  (if (now-org-stuck-project-p)
      (save-excursion (or (outline-next-heading) (point-max)))))

(declare-function org-clock-special-range "org-clock.el")
(defvar org-clock-file-total-minutes)
(defun now-org-agenda-skip-unless-archival ()
  ""
  (if (or (now-org-project-task-p)
          (and (now-org-project-p)
               (> (save-excursion
                    (save-restriction
                      (org-narrow-to-subtree)
                      (org-clock-sum (car (org-clock-special-range 'lastmonth))
                                     (cadr  (org-clock-special-range 'thismonth)))
                      org-clock-file-total-minutes))
                  0)))
      (save-excursion (org-end-of-subtree))))

(defun now-org-agenda-set-restriction-lock-to-file ()
  ""
  (interactive)
  (now-org-agenda-set-restriction-lock t))

(defun now-org-agenda-get-pom-dwim ()
  (or (org-get-at-bol 'org-hd-marker)
      (and (marker-position org-agenda-restrict-begin) org-agenda-restrict-begin)
      (and (equal major-mode 'org-mode) (point))
      org-clock-marker))

;; TODO Don’t continue up tree for 'project, stop at first parent project.
(defun now-org-agenda-set-restriction-lock (&optional type)
  ""
  (interactive "P")
  (let ((pom (now-org-agenda-get-pom-dwim)))
    (when pom
      (let ((headline (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
        (org-with-point-at pom
          (when (eq type 'project)
            (save-restriction
              (widen)
              (let ((p (save-excursion (org-back-to-heading t) (point))))
                (while (org-up-heading-safe)
                  (when (org-get-todo-state)
                    (setq p (point))))
                (goto-char p)))
            (setq type 'subtree))
          (unless (or (eq type 'file) (and (listp type) (numberp (car type))))
            (widen)
            (org-narrow-to-subtree))
          (org-agenda-set-restriction-lock type))
        (if (re-search-forward (concat "^" (regexp-quote headline) "$") nil t)
            (beginning-of-line))))))

(defun now-org-agenda-set-restriction-lock-to-project ()
  ""
  (interactive)
  (now-org-agenda-set-restriction-lock 'project))

(define-key org-agenda-mode-map "n" 'org-agenda-next-item)
(define-key org-agenda-mode-map "p" 'org-agenda-previous-item)
(define-key org-agenda-mode-map "F" 'now-org-agenda-set-restriction-lock-to-file)
(define-key org-agenda-mode-map "N" 'now-org-agenda-set-restriction-lock)
(define-key org-agenda-mode-map "P" 'now-org-agenda-set-restriction-lock-to-project)
(define-key org-agenda-mode-map "W" 'org-agenda-remove-restriction-lock)
