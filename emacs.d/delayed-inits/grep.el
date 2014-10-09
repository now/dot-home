(grep-apply-setting 'grep-command "grep -nH -P -e ")
(eval-after-load 'evil
  '(evil-make-overriding-map grep-mode-map nil))
