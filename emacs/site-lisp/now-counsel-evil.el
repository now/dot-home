(require 'evil-commands)
(require 'ivy)

(defun counsel-evil-registers-action (s)
  "Use register in S for next evil command."
  (with-ivy-window
    (evil-use-register
     (string-to-char (replace-regexp-in-string
                      "\\`\\[\\(.*?\\)\\]: .*" "\\1" s)))
    (setq this-command 'evil-use-register)))

(provide 'now-counsel-evil)
