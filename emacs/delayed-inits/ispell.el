(setq-default ispell-check-comments t)
(setq ispell-local-dictionary-alist
      '((nil "[[:alpha:]]" "[^[:alpha:]]" "['’]" nil ("-B") nil utf-8)
        ("en_US" "[[:alpha:]]" "[^[:alpha:]]" "['’]" nil ("-B") nil utf-8)
        ("en_GB-ise" "[[:alpha:]]" "[^[:alpha:]]" "['’]" nil ("-B") nil utf-8)
        ("sv_SE" "[[:alpha:]]" "[^[:alpha:]]" "['’]" nil ("-C") nil utf-8)))
