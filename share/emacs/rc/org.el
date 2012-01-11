;; TODO: Remove this require once Org 7.7 is included in Emacs
(require 'org-capture)
(setq org-directory "~/Dropbox/Org"
      org-mobile-directory "~/Dropbox/MobileOrg"
      org-mobile-force-id-on-agenda-items nil
      org-refile-targets '((org-agenda-files . (:level . 2)))
      org-refile-allow-creating-parent-nodes 'confirm
      org-outline-path-complete-in-steps nil
      org-completion-use-ido t
      org-edit-src-persistent-message nil
      org-src-window-setup 'current-window
      org-src-fontify-natively t
      org-reverse-note-order t)
(labels ((org-file (file)
                   (concat (file-name-as-directory org-directory) file)))
  (setq org-mobile-inbox-for-pull (org-file "from-mobile.org")
        org-default-notes-file (org-file "refile.org")
        org-agenda-files (list org-directory))
  (setq org-capture-templates
        '(("t" "Todo" entry (file "") "* TODO %?\n  %U\n  %i"))
        '(("T" "Annotated Todo" entry (file "") "* TODO %?\n  %U\n  %i\n  %a"))))

(add-hook 'org-mode-hook
          '(lambda ()
             (evil-define-key 'motion org-mode-map (kbd "RET") 'org-cycle)
             (evil-define-key 'normal org-mode-map ",<" 'org-mobile-pull)
             (evil-define-key 'normal org-mode-map ",>" 'org-mobile-push)
             (evil-define-key 'normal org-mode-map ",t" 'org-todo)))
