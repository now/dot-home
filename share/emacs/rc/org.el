(setq org-directory "~/Documents/Org"
      org-mobile-directory "~/Dropbox/MobileOrg")
(labels ((org-file (file)
                   (concat (file-name-as-directory org-directory) file)))
  (setq org-mobile-inbox-for-pull (org-file "flagged.org"))
  (setq org-agenda-files (list (org-file "todo.org"))))
