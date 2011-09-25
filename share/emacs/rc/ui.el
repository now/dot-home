(dolist (ui (list 'scroll-bar-mode 'tool-bar-mode))
  (if (fboundp ui)
    (funcall ui -1)))
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default show-trailing-whitespace t)
(setq initial-scratch-message nil)

(enable-theme 'now)

(setq eol-mnemonic-unix ""
      eol-mnemonic-mac "mac"
      eol-mnemonic-dos "dos"
      eol-mnemonic-undecided "?")

(setq-default mode-line-remote '(:eval (if (file-remote-p default-directory) "@" ""))
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
