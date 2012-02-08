(add-hook 'c-mode-hook
          '(lambda ()
             (define-key evil-insert-state-local-map "\C-m" 'c-context-line-break)))
; TODO: Do we need this in a hook?
(add-hook 'c-mode-common-hook
          '(lambda ()
             (c-toggle-hungry-state 1)
             (c-toggle-auto-newline 1)))

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
                             (other . "now-c-style")))))
