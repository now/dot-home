(setq-default indicate-buffer-boundaries '((bottom . left))
              mode-line-buffer-identification (propertized-buffer-identification "%b")
              mode-line-format '(""
                                 mode-line-buffer-identification
                                 (:propertize " " 'help-echo help-echo)
                                 mode-line-modes)
              show-trailing-whitespace t)
(setq eol-mnemonic-unix ""
      eol-mnemonic-mac "␍"
      eol-mnemonic-dos "␍␊"
      eol-mnemonic-undecided "?"
      mode-line-modes (butlast mode-line-modes)
      overlay-arrow-string "⇒")
(hide-mode-line)
(show-paren-mode 1)
(set-terminal-parameter nil 'background-mode 'light)
(load-theme 'now t)
(ido-mode 1)
(ido-ubiquitous-mode 1)
(flx-ido-mode 1)
(setq gc-cons-threshold 20000000)
(smex-initialize)
