(ido-mode 1)
(ido-everywhere 1)
(setq ido-enable-flex-matching t
      ido-enable-last-directory-history nil
      ido-auto-merge-work-directories-length -1
      ido-use-filename-at-point nil)
; ido-enter-matching-directory t?
(add-hook 'ido-setup-hook
          (lambda ()
            (define-key ido-completion-map "\C-p" 'ido-prev-match)
            (define-key ido-completion-map "\C-n" 'ido-next-match)
            (define-key ido-completion-map [remap backward-delete-char-untabify] 'ido-delete-backward-updir)))
