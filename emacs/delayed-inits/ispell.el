(setq-default ispell-check-comments 'exclusive)
(setq ispell-local-dictionary-alist
      '((nil "[[:alpha:]]" "[^[:alpha:]]" "['’]" nil ("-B") nil utf-8)
        ("american" "[[:alpha:]]" "[^[:alpha:]]" "['’]" nil ("-B") nil utf-8)
        ("british" "[[:alpha:]]" "[^[:alpha:]]" "['’]" nil ("-B") nil utf-8)
        ("svenska" "[[:alpha:]]" "[^[:alpha:]]" "['’]" nil ("-C") nil utf-8)))
