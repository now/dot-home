(setq org-capture-templates
      '(("c" "Chat" entry (file "")
         "* CHAT with %? :CHAT:\n%U" :clock-in t :clock-resume t)
        ("p" "Phone Call" entry (file "")
         "* CALL %? :CALL:\n%U" :clock-in t :clock-resume t)
        ("t" "Todo" entry (file "")
         "* TODO %?\n  %U\n  %i" :clock-in t :clock-resume t)
        ("T" "Annotated Todo" entry (file "")
         "* TODO %?\n  %U\n  %i\n  %a" :clock-in t :clock-resume t)))
