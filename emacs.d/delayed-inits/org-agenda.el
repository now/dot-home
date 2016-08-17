(require 'now-org)

(let* ((commands '((agenda    "")
                   (tags      "+REFILE"
                              ((org-agenda-overriding-header "Refilables")
                               (org-tags-match-list-sublevels nil)))
                   (tags-todo "-HOLD-DLGT-NIXD"
                              ((org-agenda-overriding-header "Stuck Projects")
                               (org-agenda-skip-function 'now-org-agenda-skip-unless-stuck-project)))
                   (tags-todo "-REFILE-HOLD-DLGT-WAIT-NIXD"
                              ((org-agenda-overriding-header "Tasks")
                               (org-agenda-skip-function 'now-org-agenda-skip-stuck-projects)
                               (org-agenda-sorting-strategy '((tags user-defined-up)))
                               (org-agenda-cmp-user-defined 'now-org-cmp-projects)
                               (org-tags-match-list-sublevels 'indented)
                               (org-agenda-todo-ignore-with-date t)))
                   (tags-todo "+WAIT-DLGT-NIXD|+HOLD-DLGT-NIXD"
                              ((org-agenda-overriding-header "Waiting and Held")
                               (org-tags-match-list-sublevels nil)
                               (org-agenda-todo-ignore-scheduled t)
                               (org-agenda-todo-ignore-deadlines t)))
                   (tags-todo "+DLGT"
                              ((org-agenda-overriding-header "Delegated")
                               (org-agenda-todo-match-list-sublevels nil)
                               (org-agenda-todo-ignore-scheduled t)
                               (org-agenda-todo-ignore-deadlines t)))))
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
                                     ("A" "Archivals"
                                      ((tags "-REFILE/DONE|NIXD"
                                              ((org-agenda-overriding-header "Archivals")
                                               (org-agenda-skip-function 'now-org-agenda-skip-unless-archival)
                                               (org-tags-match-list-sublevels nil)))))
                                     ("p" "Personal Agenda"
                                      ,(funcall tag-commands "personal")
                                      ((org-agenda-hide-tags-regexp "\\`personal\\'")))
                                     ("w" "Work Agenda"
                                      ,(funcall tag-commands "work")
                                      ((org-agenda-hide-tags-regexp "\\`work\\'"))))
        org-agenda-deadline-leaders '("Deadline:  " "In %d days: " "%d days ago: ")
        org-agenda-diary-file (concat (file-name-as-directory org-directory) "diary.org")
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
        org-agenda-remove-tags t
        org-agenda-sorting-strategy '((agenda habit-down time-up priority-down effort-up timestamp-down category-keep)
                                      (todo priority-down effort-up timestamp-down category-up)
                                      (tags priority-down effort-up timestamp-down category-up)
                                      (search category-up))
        org-agenda-span 'day
        org-agenda-sticky t
        org-agenda-time-leading-zero t
        org-agenda-use-time-grid nil))

(defun now-org-agenda-skip-unless-stuck-project ()
  "Skip tasks that are not `now-org-stuck-project-p'."
  (unless (now-org-stuck-project-p)
    (save-excursion (or (outline-next-heading) (point-max)))))

(defun now-org-agenda-skip-unless-active-project ()
  "Skip tasks that are not `now-org-active-project-p'."
  (unless (now-org-active-project-p)
    (save-excursion (or (outline-next-heading) (point-max)))))

(defun now-org-agenda-skip-projects ()
  "Skip tasks that aren’t tasks, depending on context.
If `org-agenda-overriding-restriction' is 'subtree,
`now-org-agenda-skip-unless-project-task' is used.  Otherwise,
`now-org-agenda-skip-unless-standalone-task' is used."
  (when (now-org-project-p)
    (save-excursion (or (outline-next-heading) (point-max)))))

(defun now-org-agenda-skip-stuck-projects ()
  "Skip tasks that are `now-org-stuck-project-p'."
  (if (now-org-stuck-project-p)
      (save-excursion (org-end-of-subtree t))))

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

(defun now-org-agenda-set-restriction-lock-to-file ()
  "Restrict agenda to file of current headline."
  (interactive)
  (now-org-agenda-set-restriction-lock t))

(defun now-org-agenda-get-pom-dwim ()
  "Get the point or mark to use for agenda functions.  This will
either be the marker representing the headline of the agenda
line, the position of the `org-agenda-restrict-begin' marker,
`point', or `org-clock-marker'."
  (or (org-get-at-bol 'org-hd-marker)
      (and (marker-position org-agenda-restrict-begin) org-agenda-restrict-begin)
      (and (equal major-mode 'org-mode) (point))
      org-clock-marker))

(defun now-org-agenda-set-restriction-lock (&optional type)
  "Restrict agenda to subtree, project, or file of current
  headline.  Restriction will be to the file if TYPE is `file' or
  if TYPE is the universal prefix '(4).  Otherwise, if TYPE is
  `project', it will be restricted to the project subtree.
  Otherwise, restriction will be to the subtree."
  (interactive "P")
  (let ((pom (now-org-agenda-get-pom-dwim)))
    (when pom
      (let* ((headline (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
             (headline-re (concat "^" (regexp-quote headline) "$"))
             (name org-agenda-this-buffer-name))
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
          (let ((org-agenda-buffer-name name))
            (org-agenda-set-restriction-lock type)))
        (if (or (re-search-backward headline-re nil t)
                (re-search-forward headline-re nil t))
            (beginning-of-line))))))

(defun now-org-agenda-set-restriction-lock-to-project ()
  "Restrict agenda to project of current headline."
  (interactive)
  (now-org-agenda-set-restriction-lock 'project))

(defun now-org-narrow-up ()
  "Narrow to parent of current headline."
  (interactive)
  (widen)
  (save-excursion
    (outline-up-heading 1 'invisible-ok)
    (org-narrow-to-subtree)
    (save-restriction
      (org-agenda-set-restriction-lock))))

(defun now-org-agenda-narrow-up ()
  "Narrow to parent of current headline in the agenda."
  (interactive)
  (let ((pom (now-org-agenda-get-pom-dwim)))
    (when pom
      (let* ((headline (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
             (headline-re (concat "^" (regexp-quote headline) "$"))
             (name org-agenda-this-buffer-name))
        (org-with-point-at pom
          (let ((org-agenda-buffer-name name))
            (now-org-narrow-up)))
        (if (or (re-search-backward headline-re nil t)
                (re-search-forward headline-re nil t))
            (beginning-of-line))))))

(defun now-org-agenda-goto-first-item-in-block (n)
  "Go to the first item of the Nth current or previous agenda block."
  (interactive "p")
  (dotimes (i n)
    (let* ((q (save-excursion
                    (when (or (not (zerop i))
                              (eq (previous-single-property-change
                                   (point-at-bol)
                                   'org-agenda-structural-header)
                                  (save-excursion
                                    (forward-visible-line -1)
                                    (point-at-eol))))
                      (org-agenda-previous-item 1))
                    (point-at-bol)))
           (p (previous-single-property-change
               q
               'org-agenda-structural-header)))
      (if (not p)
          (goto-char q)
        (goto-char p)
        (move-beginning-of-line 2))))
  (org-agenda-do-context-action))

(defun now-org-agenda-goto-last-item-in-block (n)
  "Go to the last item of the Nth current or next agenda block."
  (interactive "p")
   (dotimes (i n)
     (let ((p (or (next-single-property-change
                   (save-excursion
                     (when (or (not (zerop i))
                               (eq (next-single-property-change
                                    (point-at-eol)
                                    'org-agenda-structural-header)
                                   (save-excursion
                                     (forward-visible-line 1)
                                     (point))))
                       (org-agenda-next-item 1))
                     (point-at-eol))
                   'org-agenda-structural-header)
                  (point-max))))
       (goto-char p)
       (move-beginning-of-line 0)))
   (org-agenda-do-context-action))

(define-key org-agenda-mode-map "`" 'smex)
(define-key org-agenda-mode-map "\M-d" 'smex)
(define-key org-agenda-mode-map "d" 'now-org-agenda-next-time-span)
(define-key org-agenda-mode-map "n" 'org-agenda-next-item)
(define-key org-agenda-mode-map "p" 'org-agenda-previous-item)
(define-key org-agenda-mode-map "F" 'now-org-agenda-set-restriction-lock-to-file)
(define-key org-agenda-mode-map "N" 'now-org-agenda-set-restriction-lock)
(define-key org-agenda-mode-map "P" 'now-org-agenda-set-restriction-lock-to-project)
(define-key org-agenda-mode-map "w" 'now-org-agenda-narrow-up)
(define-key org-agenda-mode-map "W" 'org-agenda-remove-restriction-lock)
(define-key org-agenda-mode-map "(" 'now-org-agenda-goto-first-item-in-block)
(define-key org-agenda-mode-map ")" 'now-org-agenda-goto-last-item-in-block)

(defun now-turn-on-hl-line-mode ()
  "Turn on `hl-line-mode'."
  (hl-line-mode 1))

(add-hook 'org-agenda-mode-hook 'now-turn-on-hl-line-mode)
