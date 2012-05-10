; TODO: Add (add-to-list 'c-cleanup-list 'defun-close-semi)?
; We don’t need it right now.

(eval-after-load 'cc-mode
  '(progn
     (defconst now-c-style
       '("linux"
         (c-hanging-braces-alist . ((brace-list-open)
                                    (brace-list-close)
                                    (brace-entry-open)
                                    (substatement-open after)
                                    (block-close . c-snug-do-while)
                                    (arglist-cont-nonempty)
                                    (class-close)))
         (c-hanging-colons-alist . ((case-label after)
                                    (label after))))
       "now’s C Programming Style")
     (c-add-style "now-c-style" now-c-style)
     (setq c-default-style '((java-mode . "java")
                             (awk-mode . "awk")
                             (other . "now-c-style")))
     (define-key c-mode-base-map "\C-j" 'c-context-line-break)))
