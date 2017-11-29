(setq completion-show-help nil
      mail-user-agent 'mu4e-user-agent)
(add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode)
