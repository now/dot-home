(setq-default sh-indentation 2
              sh-basic-offset 2)
(define-key sh-mode-map "\C-j" 'reindent-then-newline-and-indent)
(add-to-list 'sh-alias-alist '(@SHELL@ . sh))
(add-to-list 'sh-alias-alist '(@ZSHELL@ . zsh))
