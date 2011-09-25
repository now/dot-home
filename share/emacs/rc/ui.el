(dolist (ui (list 'scroll-bar-mode 'tool-bar-mode))
  (if (fboundp ui)
    (funcall ui -1)))
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default show-trailing-whitespace t)
(setq initial-scratch-message nil)

(enable-theme 'now)
