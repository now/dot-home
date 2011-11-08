(setq org-directory "~/Dropbox/Org"
      org-mobile-directory "~/Dropbox/MobileOrg"
      org-refile-targets '((org-agenda-files . (:level . 1))))
(labels ((org-file (file)
                   (concat (file-name-as-directory org-directory) file)))
  (setq org-mobile-inbox-for-pull (org-file "from-mobile.org"))
  (setq org-agenda-files (list org-directory)))

(add-hook 'org-mode-hook
          '(lambda ()
             (evil-define-key 'motion org-mode-map (kbd "RET") 'org-cycle)
             (evil-define-key 'normal org-mode-map ",<" 'org-mobile-pull)
             (evil-define-key 'normal org-mode-map ",>" 'org-mobile-push)
             (evil-define-key 'normal org-mode-map ",t" 'org-todo)))
