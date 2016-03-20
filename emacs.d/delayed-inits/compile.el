(setq compilation-scroll-output 'first-error)

(add-to-list 'compilation-error-regexp-alist-alist
             '(autotest-summary
               "^\\(?:  [0-9]\\| [0-9][0-9]\\|[0-9][0-9][0-9]\\): .*\\(?: FAILED\\| WARNIN\\(G\\)\\) (\\([^\n :]+\\.at\\):\\([1-9][0-9]*\\))"
               2 3 nil (1 . nil)))
(add-to-list 'compilation-error-regexp-alist 'autotest-summary)

(add-to-list 'compilation-error-regexp-alist-alist
             '(autotest-header
               "^\\([1-9][0-9]*\\. \\([^\n :]+\\.at\\):\\([1-9][0-9]*\\)\\): \\(?: FAILED\\|WARNIN\\(G\\)\\|\\(testing\\| ok\\)\\)"
               2 3 nil (4 . 5) 1))
(add-to-list 'compilation-error-regexp-alist 'autotest-header)

(add-to-list 'compilation-error-regexp-alist-alist
             '(autotest-check
               "^\\(\\([^\n :]+\\.at\\):\\([1-9][0-9]*\\)\\): "
               2 3 nil 0 1))
(add-to-list 'compilation-error-regexp-alist 'autotest-check)

(add-to-list 'compilation-error-regexp-alist-alist
             '(autotest-check-error
               "^\\(\\([^\n :]+\\.at\\):\\([1-9][0-9]*\\)\\): [^\n]+\n--- "
               2 3 nil 2 1))
(add-to-list 'compilation-error-regexp-alist 'autotest-check-error)

(add-to-list 'compilation-error-regexp-alist-alist
             '(gnu
               "^\\(?:[[:alpha:]][-[:alnum:].]+: ?\\|[ \t]+\\(?:in \\|from \\)\\)?\
\\([0-9]*[^0-9\n]\\(?:[^\n :]\\| [^-/\n]\\|:[^ \n]\\)*?\\): ?\
\\([0-9]+\\)\\(?:[.:]\\([0-9]+\\)\\)?\
\\(?:-\\([0-9]+\\)?\\(?:\\.\\([0-9]+\\)\\)?\\)?:\
\\(?: *\\(\\(?:Future\\|Runtime\\)?[Ww]arning\\|W:\\)\\|\
 *\\([Ii]nfo\\(?:\\>\\|rmationa?l?\\)\\|I:\\|instantiated from\\|[Nn]ote\\)\\|\
 *[Ee]rror\\|\[0-9]?\\(?:[^0-9\n]\\|$\\)\\|[0-9][0-9][0-9]\\)"
               1 (2 . 4) (3 . 5) (6 . 7)))

(add-to-list 'compilation-error-regexp-alist-alist
             '(ruby-backtrace
               "^[ \t]+\\(?:in \\|from \\)\
\\([0-9]*[^0-9\n]\\(?:[^\n :]\\| [^-/\n]\\|:[^ \n]\\)*?\\):\
\\([0-9]+\\)\\(?::in .*\\)?"
               1 2 nil 0))
(add-to-list 'compilation-error-regexp-alist 'ruby-backtrace)
