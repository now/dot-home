(setq org-directory "~/Documents/Org"
      org-mobile-directory "~/Dropbox/MobileOrg"
      org-refile-targets '((org-agenda-files . (:level . 1))))
(labels ((org-file (file)
                   (concat (file-name-as-directory org-directory) file)))
  (setq org-mobile-inbox-for-pull (org-file "from-mobile.org"))
  (setq org-agenda-files (list
                          (org-file "music.org")
                          (org-file "todo.org"))))
