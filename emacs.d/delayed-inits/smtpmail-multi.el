(setq smtpmail-multi-accounts '((amesto . (nil "smtp-z91.telecomputing.no" 587
                                           nil nil nil nil nil))
                                (disuse . ("now@disu.se" "disu.se" 587
                                           nil nil nil nil nil)))
      smtpmail-multi-associations '(("nikolai.weibull@amesto.se" amesto)
                                    ("now@disu.se" disuse)))
