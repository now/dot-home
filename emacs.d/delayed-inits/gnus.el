(require 'nnir)
(setq gnus-ancient-mark ?\s
      gnus-fetch-old-headers t
      gnus-parameters '((".*"
                         (gnus-use-scoring nil)
                         (display . all))
                        ("^nnimap\\+work:"
                         (posting-style
                          (address "nikolai.weibull@amesto.se")
                          (gcc "nnimap+work:\"Sent Items\""))))
      gnus-permanently-visible-groups "INBOX\\'"
      ;; TODO Should ssl perhaps be starttls?
      gnus-secondary-select-methods '((nnimap "work"
                                              (nnimap-address "imap-z91.telecomputing.no")
                                              (nnimap-stream ssl)))
      gnus-select-method '(nnimap "gmail"
                                  (nnimap-address "imap.gmail.com")
                                  (nnimap-stream ssl))
      gnus-summary-line-format "%U%R%z%>%(%*%-23,23f%)  %s%-67=  %11&user-date;\n"
      gnus-thread-sort-functions '((not gnus-thread-sort-by-number))
      gnus-unread-mark ?\·
      gnus-use-correct-string-widths t
      gnus-user-date-format-alist '(((gnus-seconds-today) . "%H:%M")
                                    ((+ 86400 (gnus-seconds-today)) . "%a %H:%M")
                                    (604800 . "%a, %b %-d")
                                    (15778476 . "%b %-d")
                                    (t . "%Y-%m-%d")))
; TODO This should be set per method, and to the right folder.
;(setq gnu-message-archive-method nil)

