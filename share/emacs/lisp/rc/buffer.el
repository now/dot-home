(setq-default indicate-buffer-boundaries '((bottom . left))
              mode-line-buffer-identification (propertized-buffer-identification "%b")
              mode-line-modes (butlast mode-line-modes)
              mode-line-format '(""
                                 mode-line-client
                                 mode-line-buffer-identification
                                 (:propertize " " 'help-echo help-echo)
                                 mode-line-modes))
