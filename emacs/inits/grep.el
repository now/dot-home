(grep-apply-setting 'grep-command "grep -nH -P -e ")
(eval-after-load 'evil
  '(evil-define-key 'normal grep-mode-map "q" 'close-buffer-and-window-unless-last))
