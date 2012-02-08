(eval-after-load 'grep
  '(progn
     (grep-apply-setting 'grep-command "grep -nH -P -e ")
     (evil-define-key 'normal grep-mode-map "q" 'close-buffer-and-window-unless-last)))
