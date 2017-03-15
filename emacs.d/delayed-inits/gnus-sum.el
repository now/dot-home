(setq gnus-ancient-mark ?\s
      gnus-fetch-old-headers t
      gnus-thread-sort-functions '((not gnus-thread-sort-by-number))
      gnus-unread-mark ?\·
      gnus-user-date-format-alist '(((gnus-seconds-today) . "%H:%M")
                                    ((+ 86400 (gnus-seconds-today)) . "%a %H:%M")
                                    (604800 . "%a, %b %-d")
                                    (15778476 . "%b %-d")
                                    (t . "%Y-%m-%d")))

(add-hook 'gnus-summary-mode-hook 'hl-line-mode)
