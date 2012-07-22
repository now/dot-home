(eval-after-load 'org
  '(progn
     (setq org-directory "~/Dropbox/Org"
           org-mobile-directory "~/Dropbox/MobileOrg"
           org-mobile-force-id-on-agenda-items nil
           org-refile-targets '((org-agenda-files . (:level . 1)))
           org-refile-allow-creating-parent-nodes 'confirm
           org-outline-path-complete-in-steps nil
           org-completion-use-ido t
           org-edit-src-persistent-message nil
           org-src-window-setup 'current-window
           org-src-fontify-natively t
           org-reverse-note-order t
           org-log-done 'time)
     (flet ((org-file (file)
                      (concat (file-name-as-directory org-directory) file)))
       (setq org-mobile-inbox-for-pull (org-file "from-mobile.org")
             org-default-notes-file (org-file "refile.org")
             org-agenda-files (list org-directory))
       (setq org-capture-templates
             '(("t" "Todo" entry (file "") "* TODO %?\n  %U\n  %i")
               ("T" "Annotated Todo" entry (file "") "* TODO %?\n  %U\n  %i\n  %a"))))))

(eval-after-load 'evil-maps
  '(progn
     (evil-declare-key 'motion org-mode-map
       (kbd "RET") 'org-cycle)
     (evil-declare-key 'normal org-mode-map
       ",<" 'org-mobile-pull
       ",>" 'org-mobile-push
       ",t" 'org-todo)))
