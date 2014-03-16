(defun now-refile-target-verify ()
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))
(setq ;org-agenda-span 'day
      org-archive-default-command 'org-archive-to-archive-sibling
      org-clock-out-remove-zero-time-clocks t
      org-directory "~/Documents/Google Drive/Org"
      org-enforce-todo-dependencies t
      org-link-frame-setup '((vm . vm-visit-folder)
                             (gnus . org-gnus-no-new-news)
                             (file . find-file)
                             (wl . wl))
      org-loop-over-headlines-in-active-region 'region-start-level
      org-mobile-directory "~/Sites/dav"
      org-mobile-force-id-on-agenda-items nil
      org-refile-allow-creating-parent-nodes 'confirm
      org-refile-target-verify-function 'now-refile-target-verify
      org-refile-targets '((org-agenda-files . (:maxlevel . 9)))
      org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil
      org-completion-use-ido t
      org-edit-src-persistent-message nil
      org-src-window-setup 'current-window
      org-src-fontify-natively t
      org-treat-S-cursor-todo-selection-as-state-change nil
      org-reverse-note-order t
      org-log-done 'time
      org-log-into-drawer t)

;; TODO Move these to files so that MobileOrg and similar may see them
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAIT(w@/!)" "DLGT(g@/!)" "|" "DONE(d)" "NIXD(c@/!)")
        (type "CALL")
        (type "CHAT")))
(setq org-todo-keyword-faces
      '(("DLGT" . org-delegated)
        ("WAIT" . org-waiting)))
(let ((org-file (lambda (file)
                  (concat (file-name-as-directory org-directory) file))))
  (setq org-mobile-inbox-for-pull (funcall org-file "from-mobile.org")
        org-default-notes-file (funcall org-file "refile.org")
        org-agenda-files (list org-directory)))
(setq org-capture-templates
      '(("c" "Chat" entry (file "")
         "* CHAT with %? :CHAT:\n%U" :clock-in t :clock-resume t)
        ("p" "Phone Call" entry (file "")
         "* CALL %? :CALL:\n%U" :clock-in t :clock-resume t)
        ("t" "Todo" entry (file "")
         "* TODO %?\n  %U\n  %i" :clock-in t :clock-resume t)
        ("T" "Annotated Todo" entry (file "")
         "* TODO %?\n  %U\n  %i\n  %a" :clock-in t :clock-resume t)))

(add-hook 'org-mode-hook 'turn-on-auto-compile-project-mode)

(eval-after-load 'evil
  '(progn
     (evil-define-key 'motion org-mode-map
       (kbd "RET") 'org-cycle)
     (evil-define-key 'normal org-mode-map
       ",<" 'org-mobile-pull
       ",>" 'org-mobile-push
       ",t" 'org-todo)))
