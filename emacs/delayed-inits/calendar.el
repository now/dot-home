(setq calendar-mark-holidays-flag t
      calendar-intermonth-text '(propertize
                                 (format "%2d"
                                         (car
                                          (calendar-iso-from-absolute
                                           (calendar-absolute-from-gregorian (list month day year)))))
                                 'font-lock-face 'week)
      calendar-week-start-day 1)