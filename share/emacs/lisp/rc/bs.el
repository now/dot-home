(eval-after-load 'bs
  '(progn
     (evil-make-overriding-map bs-mode-map 'normal t)
     (evil-define-key 'motion bs-mode-map "h" 'evil-backward-char)
     (evil-define-key 'motion bs-mode-map "j" 'bs-down)
     (evil-define-key 'normal bs-mode-map "k" 'bs-up)
     (evil-define-key 'motion bs-mode-map "k" 'bs-up)
     (evil-define-key 'normal bs-mode-map "s" 'evil-forward-char)
     (evil-define-key 'motion bs-mode-map "s" 'evil-forward-char)
     (evil-define-key 'normal bs-mode-map "w" 'bs-save)))
