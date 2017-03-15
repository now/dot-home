(require 'dired-x)

(setq dired-dwim-target t
      dired-listing-switches "--si -al"
      dired-recursive-copies 'always
      dired-recursive-deletes 'always)

(add-hook 'dired-mode-hook 'hl-line-mode)
