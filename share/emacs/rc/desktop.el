(eval-after-load 'desktop
  '(progn
     (setq desktop-path '("~/.cache/emacs")
            desktop-dirname (car desktop-path))
     (dolist (variable '(command-history
                         compile-history
                         evil-ex-history
                         evil-ex-search-history
                         read-expression-history
                         shell-command-history))
       (add-to-list 'desktop-globals-to-save variable))))
(desktop-save-mode 1)
