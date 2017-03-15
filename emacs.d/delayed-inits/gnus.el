(require 'nnir)
(setq gnus-parameters '((".*"
                         (gnus-use-scoring nil)
                         (display . all))
                        ("^nnimap\\+work:"
                         (expiry-target "nnimap+work:\"Deleted Items\"")
                         (posting-style
                          (address "nikolai.weibull@amesto.se")
                          (gcc "nnimap+work:\"Sent Items\"")
                          (signature "Nikolai Weibull\nSystems Developer\nOffice: +46 31-360 98 39 | Amesto Translations"))))

      ;; TODO Should ssl perhaps be starttls?
      gnus-secondary-select-methods '((nnimap "gmail"
                                              (nnimap-address "imap.gmail.com")
                                              (nnimap-stream ssl))
                                      (nnimap "work"
                                              (nnimap-address "imap-z91.telecomputing.no")
                                              (nnimap-stream ssl)))
      gnus-select-method '(nnnil)
      gnus-summary-line-format "%U%R%z%>%(%*%-23,23f%)  %s%-67=  %11&user-date;\n")

(gnus-add-configuration
 '(article (vertical 1.0
                     (summary 0.5 point)
                     (article 1.0))))
