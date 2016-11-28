(setq desktop-base-file-name "emacs.desktop"
      desktop-dirname (car desktop-path)
      desktop-restore-eager nil)
(dolist (variable '(command-history
                    compile-history
                    evil-ex-history
                    evil-ex-search-history
                    read-expression-history
                    shell-command-history))
  (add-to-list 'desktop-globals-to-save variable))
