(eval-after-load 'hideshow
  '(setq hs-set-up-overlay (lambda (ov)
                             (overlay-put ov 'display " …"))))
