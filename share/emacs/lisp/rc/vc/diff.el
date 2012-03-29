(eval-after-load 'diff
  '(progn
     (setq diff-switches "-u")
     (add-hook 'diff-mode-hook 'now-do-not-show-trailing-whitespace)))
(eval-after-load 'evil-core
  '(evil-declare-key 'normal diff-mode-map "q" 'close-buffer-and-window-unless-last))
