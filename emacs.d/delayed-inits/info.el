(add-to-list 'Info-additional-directory-list
             (concat user-emacs-directory "info"))
(add-hook 'Info-mode-hook 'now-do-not-show-trailing-whitespace)
