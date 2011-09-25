(setq-default indicate-buffer-boundaries '((bottom . left))
              mode-line-remote '(:eval (if (file-remote-p default-directory) "@" ""))
              mode-line-buffer-identification (propertized-buffer-identification "%b")
              mode-line-frame-identification '(:eval (if (or (null window-system) (eq window-system 'pc)) "[%F] " ""))
              mode-line-modes (butlast mode-line-modes)
              ; TODO: 'mode-line-remote might be overkill.  Is it really
              ; pertinent information?
              mode-line-format (list
                                 ""
                                 'mode-line-client
                                 'mode-line-remote
                                 'mode-line-frame-identification
                                 'mode-line-buffer-identification
                                 '(:propertize " " 'help-echo help-echo)
                                 'mode-line-modes))
