(defun now-tabulated-list-mode-use-global-glyphless-char-display ()
  (kill-local-variable 'glyphless-char-display))
(add-hook 'tabulated-list-mode-hook
          'now-tabulated-list-mode-use-global-glyphless-char-display)
