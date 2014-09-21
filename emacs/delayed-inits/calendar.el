(setq calendar-date-style 'iso
      calendar-mark-holidays-flag t
      calendar-intermonth-text '(propertize
                                 (format "%2d"
                                         (car
                                          (calendar-iso-from-absolute
                                           (calendar-absolute-from-gregorian (list month day year)))))
                                 'font-lock-face 'week)
      calendar-week-start-day 1)
(eval-after-load 'evil
  '(progn
     (evil-define-key 'motion calendar-mode-map
       "\C-b" 'calendar-scroll-right-three-months
       "\C-f" 'calendar-scroll-left-three-months
       "H" 'calendar-cursor-holidays
       "b" 'calendar-beginning-of-week
       "h" 'calendar-backward-day
       "j" 'calendar-forward-week
       "k" 'calendar-backward-week
       "s" 'calendar-forward-day
       "w" 'calendar-end-of-week)))
