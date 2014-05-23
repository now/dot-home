(setq org-agenda-clockreport-parameter-plist '(:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80)
      org-agenda-custom-commands '((" " "Agenda"
                                    ((agenda    "")
                                     (tags      "REFILE"
                                                ((org-agenda-overriding-header "Refilables")
                                                 (org-tags-match-list-sublevels nil)))
                                     (tags      "-REFILE/DONE|NIXD"
                                                ((org-agenda-overriding-header "Archivables")
                                                 (org-tags-match-list-sublevels nil))))
                                    nil))
      org-agenda-deadline-leaders '("Deadline:  " "In %d days: " "%d days ago: ")
      org-agenda-log-mode-items '(clocked closed state)
      org-agenda-prefix-format '((agenda . " %i %-13:c%?-12t% s")
                                 (timeline . "  % s")
                                 (todo . " %i %-13:c")
                                 (tags . " %i %-13:c")
                                 (search . " %i %-13:c"))
      org-agenda-span 'day
      org-agenda-use-time-grid nil)
