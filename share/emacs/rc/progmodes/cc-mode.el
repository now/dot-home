; TODO: Bind TAB to simply indent?
; TODO: It seems like Viper is getting in the way of CC mode for hungry
; submode.
(defun my-make-CR-do-indent ()
  (define-key c-mode-base-map "\C-m" 'c-context-line-break)
  (viper-add-local-keys 'insert-state '(("\C-m" . c-context-line-break))))
(add-hook 'c-initialization-hook 'my-make-CR-do-indent)
(add-hook 'c-mode-common-hook
          (lambda ()
            (c-toggle-hungry-state 1)
            (c-toggle-auto-newline 1)))

; TODO: Add (add-to-list 'c-cleanup-list 'defun-close-semi)?
; We don’t need it right now.

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