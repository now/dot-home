(setq bs-max-window-height nil)
(eval-after-load 'evil
  '(progn
     (evil-make-overriding-map bs-mode-map 'normal t)
     (evil-define-key 'normal bs-mode-map
       "w" 'bs-save)
     (evil-define-key 'motion bs-mode-map
       "h" 'evil-backward-char
       "j" 'bs-down
       "k" 'bs-up
       "k" 'bs-up
       "s" 'evil-forward-char
       "s" 'evil-forward-char)))
