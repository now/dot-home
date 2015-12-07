(setq magit-repository-directories '("~/Projects")
      magit-repository-directories-depth 1
      magit-revert-buffers 'silent)
(add-hook 'magit-mode-hook 'now-do-not-show-trailing-whitespace)
