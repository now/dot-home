(setq holiday-general-holidays '((holiday-fixed 1 1 "New Year’s Day")
                                 (holiday-fixed 2 14 "Valentine’s Day")
                                 (holiday-fixed 3 8 "International Women’s Day")
                                 (holiday-fixed 4 1 "April Fools’ Day")
                                 (holiday-fixed 5 1 "International Worker’s Day")
                                 (holiday-float 5 0 2 "Mother’s Day (US)")
                                 (holiday-float 5 0 -1 "Mother’s Day (Sweden)")
                                 (holiday-fixed 6 1 "National Day of Sweden")
                                 (let ((midsummer (calendar-dayname-on-or-before
                                                   6 (calendar-absolute-from-gregorian
                                                      (list 6 26 displayed-year)))))
                                   (filter-visible-calendar-holidays
                                    (list
                                     (list
                                      (calendar-gregorian-from-absolute (1- midsummer))
                                      "Midsummer’s Eve")
                                     (list
                                      (calendar-gregorian-from-absolute midsummer)
                                      "Midsummer"))))
                                 (holiday-float 6 0 3 "Father’s Day (US)")
                                 (holiday-fixed 7 4 "Independence Day")
                                 (holiday-fixed 10 31 "Halloween")
                                 (holiday-float 11 0 2 "Father’s Day (Sweden)")
                                 (holiday-float 11 4 4 "Thanksgiving")
                                 (holiday-fixed 12 31 "New Year’s Eve"))
      holiday-christian-holidays '((holiday-fixed 1 6 "Epiphany")
                                   (holiday-easter-etc -2 "Good Friday")
                                   (holiday-easter-etc -1 "Holy Saturday")
                                   (holiday-easter-etc 0 "Easter Sunday")
                                   (holiday-easter-etc +1 "Easter Monday")
                                   (holiday-easter-etc +39 "Ascension Day")
                                   (holiday-easter-etc +49 "Pentecost")
                                   (holiday-sexp
                                    '(calendar-gregorian-from-absolute
                                      (calendar-dayname-on-or-before
                                       6 (calendar-absolute-from-gregorian
                                          (list 11 6 year))))
                                    "All Saint’s Day")
                                   (holiday-fixed 12 13 "Saint Lucy’s Day")
                                   (holiday-fixed 12 24 "Christmas Eve")
                                   (holiday-fixed 12 25 "Christmas")
                                   (holiday-fixed 12 26 "Boxing Day"))
      calendar-holidays (append holiday-general-holidays
                                holiday-local-holidays
                                holiday-christian-holidays
                                holiday-other-holidays
                                holiday-solar-holidays))
