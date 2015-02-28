(add-hook 'magit-log-edit-mode-hook
          (lambda ()
            (setq fill-column 72)))
(magit-auto-revert-mode -1)
