(eval-after-load 'sh-script
  '(progn
     (setq sh-indentation 2
           sh-basic-offset 2)
     (define-key sh-mode-map "\C-j" 'reindent-then-newline-and-indent)))
