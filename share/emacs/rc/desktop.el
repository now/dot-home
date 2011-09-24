(desktop-save-mode 1)
(dolist (variable '(command-history
                    compile-history
                    evil-ex-history
                    evil-ex-search-history
                    read-expression-history
                    shell-command-history))
  (add-to-list 'desktop-globals-to-save variable))

(lexical-let ((global-desktop-dirname (expand-file-name "~/.cache/emacs")))
  (setq desktop-path (list "." global-desktop-dirname))
  (defun desktop-save-globally ()
    (interactive)
    (setq desktop-dirname global-desktop-dirname))
  (defun desktop-save-locally ()
    (interactive)
    (setq desktop-dirname (expand-file-name "."))))
(desktop-save-globally)
