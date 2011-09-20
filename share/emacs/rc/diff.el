(setq-default diff-switches "-u")
(add-hook 'diff-mode-hook
          (lambda ()
            (set (make-local-variable 'show-trailing-whitespace) nil)))
