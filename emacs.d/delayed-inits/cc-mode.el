(c-add-style "now-c-style"
             '("linux"
               (c-hanging-braces-alist . ((brace-entry-open)
                                          (brace-list-open after)
                                          (brace-list-close before)
                                          (class-close before)
                                          (class-open after)
                                          (substatement-open after)
                                          (block-close before)))
               (c-hanging-colons-alist . ((case-label after)
                                          (label after)))))
(c-add-style "now-java-style"
             '("java"
               (c-basic-offset . 2)))
(setq c-default-style '((java-mode . "now-java-style")
                        (awk-mode . "awk")
                        (other . "now-c-style"))
      c-electric-pound-behavior '(alignleft))
(define-key c-mode-base-map "\C-j" 'c-context-line-break)
(defun now-c-mode-adaptive-fill-function ()
  (when (looking-at "\\([ \t]*\\(//+\\|\\**\\)[ \t]*\\)@[[:alpha:]]+[ \t]")
    (concat (match-string 1) "  ")))
(defun now-c-mode-hook ()
  (set (make-local-variable 'comment-start) "// ")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'paragraph-start)
       "[ \t]*\\(//+\\|\\**\\)[ \t]*$\\|[ \t]*\\(//+\\|\\**\\)[ \t]*@[[:alpha:]]+[ \t]\\|^\f")
  (set (make-local-variable 'adaptive-fill-function)
       'now-c-mode-adaptive-fill-function)
  (c-toggle-auto-newline 1))
(add-hook 'c-mode-hook 'now-c-mode-hook)
