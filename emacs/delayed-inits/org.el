(defun now-refile-target-verify ()
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))
(setq ;org-agenda-span 'day
      org-directory "~/Documents/Org"
      org-agenda-files (list org-directory)
      org-default-notes-file (concat (file-name-as-directory org-directory) "refile.org")
      org-enforce-todo-dependencies t
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
      org-reverse-note-order t
      org-log-done 'time
      org-log-into-drawer t
      org-yank-adjusted-subtrees t
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
