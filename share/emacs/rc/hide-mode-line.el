(require 'hide-mode-line)
(hide-mode-line)
(add-hook 'window-setup-hook
          (lambda ()
            (hide-mode-line-update)))
