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

(defun now-org-find-tree (function)
  "Find the first occurrence of a FUNCTION returning a non-nil value."
  (catch 'exit
    (org-map-tree (lambda () (if (funcall function) (throw 'exit t))))))

(defun now-org-has-descendant-p (g)
  "TBD."
  (catch 'done
    (now-org-each-descendant (lambda ()
                               (if (funcall g)
                                   (throw 'done t))))))

(defun now-org-project-p ()
  "Return `t' if `point' is inside a project, that is, a headline
with a todo keyword and a sub-headline with a todo keyword.  This
function assumes that `point' is at a project task."
  (now-org-has-descendant-p (lambda () (org-get-todo-state))))

(defun now-org-stuck-project-p ()
  (and (not (now-org-active-project-p)) (now-org-project-p)))

(defun now-org-active-project-p ()
  "Return `t' if `point' is inside an active project, that is, a
project with a sub-task with a todo keyword set to NEXT that
doesn’t have a WAIT tag (inherited or direct).  This function
assumes that `point' is at a project task, that is, a headline
with a todo keyword."
  (now-org-has-descendant-p (lambda ()
                              (and (string= (org-get-todo-state) "NEXT")
                                   (not (member "WAIT" (org-get-tags-at)))))))

(defun now-org-project-task-p ()
  "Return `t' if `point' is inside a project."
  (save-excursion
    (save-restriction
      (widen)
      (and (org-up-heading-safe) (org-get-todo-state)))))

(defun now-org-standalone-task-p ()
  ""
  (not (or (now-org-project-p) (now-org-project-task-p))))

(defun now-refile-target-verify ()
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))
(setq org-directory "~/Documents/Org"
      org-agenda-deadline-faces '((0.0 . default))
      org-agenda-files (list org-directory)
      org-agenda-text-search-extra-files '(argenda-archives)
      org-catch-invisible-edits 'smart
      org-columns-default-format "%80ITEM(Task) %7Effort{:} %7CLOCKSUM(Clocked)"
      org-columns-ellipses "…"
      org-completion-use-ido t
      org-default-notes-file (concat (file-name-as-directory org-directory) "refile.org")
      org-edit-src-persistent-message nil
      org-enforce-todo-dependencies t
      org-global-properties '(("Effort_ALL" . "0:15 0:30 1:00 2:00 4:00 8:00 0:00"))
      org-link-frame-setup '((vm . vm-visit-folder)
                             (gnus . org-gnus-no-new-news)
                             (file . find-file)
                             (wl . wl))
      org-log-done 'time
      org-log-into-drawer t
      org-loop-over-headlines-in-active-region 'region-start-level
      org-outline-path-complete-in-steps nil
      org-refile-allow-creating-parent-nodes 'confirm
      org-refile-target-verify-function 'now-refile-target-verify
      org-refile-targets '((org-agenda-files . (:maxlevel . 9)))
      org-refile-use-outline-path 'file
      org-src-window-setup 'current-window
      org-tag-alist '(("personal" . ?p)
                      ("shopping" . ?s)
                      ("work" . ?w)
                      ("HOLD" . ?h)
                      ("WAIT" . ?W)
                      ("DLGT" . ?g)
                      ("NIXD" . ?c))
      org-time-clocksum-format '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)
      org-todo-keyword-faces '(("DLGT" . org-delegated)
                               ("HOLD" . org-hold)
                               ("NEXT" . org-next)
                               ("WAIT" . org-waiting))
      org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                          (sequence "HOLD(h)" "WAIT(w@/@)" "DLGT(g@/!)" "|" "NIXD(c@/!)")
                          (type "CALL")
                          (type "CHAT"))
      org-todo-state-tags-triggers '(("TODO" ("HOLD") ("WAIT") ("DLGT") ("NIXD"))
                                     ("NEXT" ("HOLD") ("WAIT") ("DLGT") ("NIXD"))
                                     ("DONE" ("HOLD") ("WAIT") ("DLGT") ("NIXD"))
                                     (done   ("HOLD") ("WAIT") ("DLGT"))
                                     ("HOLD" ("HOLD" . t))
                                     ("WAIT" ("WAIT" . t))
                                     ("DLGT" ("DLGT" . t))
                                     ("NIXD" ("NIXD" . t)))
      org-treat-S-cursor-todo-selection-as-state-change nil
      org-yank-adjusted-subtrees t)
(eval-after-load 'evil
  '(progn
     (evil-define-key 'motion org-mode-map
       (kbd "RET") 'org-cycle)
     (evil-define-key 'normal org-mode-map
       ",<" 'org-mobile-pull
       ",>" 'org-mobile-push
       ",t" 'org-todo)))

(defun now-insert-heading-inactive-timestamp ()
  (save-excursion
    (org-return)
    (org-cycle)
    (org-insert-time-stamp nil t t nil nil nil)))

(add-hook 'org-insert-heading-hook 'now-insert-heading-inactive-timestamp)

(add-hook 'org-mode-hook 'turn-on-flyspell)

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

(add-hook 'org-after-todo-state-change-hook 'now-org-adjust-properties-after-todo-state-change)

(org-clock-persistence-insinuate)
