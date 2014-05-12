(defun now-refile-target-verify ()
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))
(setq org-agenda-deadline-faces '((0.0 . default))
      org-agenda-deadline-leaders '("Deadline:  " "In %d days: " "%d days ago: ")
      org-agenda-prefix-format '((agenda . " %i %-13:c%?-12t% s")
                                 (timeline . "  % s")
                                 (todo . " %i %-13:c")
                                 (tags . " %i %-13:c")
                                 (search . " %i %-13:c"))
      org-agenda-span 'day
      org-agenda-text-search-extra-files '(argenda-archives)
      org-agenda-use-time-grid nil
      org-catch-invisible-edits 'smart
      org-columns-default-format "%80ITEM(Task) %7Effort{:} %7CLOCKSUM(Clocked)"
      org-columns-ellipses "…"
      org-directory "~/Documents/Org"
      org-agenda-files (list org-directory)
      org-default-notes-file (concat (file-name-as-directory org-directory) "refile.org")
      org-enforce-todo-dependencies t
      org-global-properties '(("Effort_ALL" . "0:15 0:30 1:00 2:00 4:00 8:00 0:00"))
      org-link-frame-setup '((vm . vm-visit-folder)
                             (gnus . org-gnus-no-new-news)
                             (file . find-file)
                             (wl . wl))
      org-loop-over-headlines-in-active-region 'region-start-level
      org-refile-allow-creating-parent-nodes 'confirm
      org-refile-target-verify-function 'now-refile-target-verify
      org-refile-targets '((org-agenda-files . (:maxlevel . 9)))
      org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil
      org-completion-use-ido t
      org-edit-src-persistent-message nil
      org-src-window-setup 'current-window
      org-treat-S-cursor-todo-selection-as-state-change nil
      org-time-clocksum-format '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)
      org-log-done 'time
      org-log-into-drawer t
      org-yank-adjusted-subtrees t
      org-tag-alist '(("shopping" . ?s))
      org-todo-keywords '((sequence "TODO(t)" "WAIT(w@/@)" "DLGT(g@/!)" "|" "DONE(d)" "NIXD(c@/!)")
                          (type "CALL")
                          (type "CHAT"))
      org-todo-keyword-faces '(("DLGT" . org-delegated)
                               ("WAIT" . org-waiting)))
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
