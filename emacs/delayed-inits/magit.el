(define-key magit-mode-map "q" 'close-buffer-and-window-unless-last)

(add-hook 'magit-log-edit-mode-hook
          (lambda ()
            (setq fill-column 72)))
