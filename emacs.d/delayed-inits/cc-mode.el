(c-add-style "now-c-style"
             '("linux"
               (c-hanging-braces-alist . ((brace-list-open)
                                          (brace-list-close)
                                          (brace-entry-open)
                                          (substatement-open after)
                                          (block-close . c-snug-do-while)
                                          (arglist-cont-nonempty)
                                          (class-close)))
               (c-hanging-colons-alist . ((case-label after)
                                          (label after)))))
(c-add-style "now-java-style"
             '("java"
               (c-basic-offset . 2)))
(setq c-default-style '((java-mode . "now-java-style")
                        (awk-mode . "awk")
                        (other . "now-c-style")))
(define-key c-mode-base-map "\C-j" 'c-context-line-break)
(add-hook 'c-mode-hook
          (lambda ()
            (set (make-local-variable 'comment-start) "// ")
            (set (make-local-variable 'comment-end) "")
            (set (make-local-variable 'paragraph-start) "[ \t]*\\(//+\\|\\**\\)[ \t]*$\\|[ \t]*\\(//+\\|\\**\\)[ \t]*@[[:alpha:]]+[ \t]\\|^\f")
            (set (make-local-variable 'adaptive-fill-function)
                 (lambda ()
                   (if (looking-at "\\([ \t]*\\(//+\\|\\**\\)[ \t]*\\)@[[:alpha:]]+[ \t]")
                       (concat (match-string 1) "  "))))))
