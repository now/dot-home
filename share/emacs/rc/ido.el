; TODO: Doesn’t seem to be needed: (require 'ido)
(ido-mode 1)
(ido-everywhere 1)
(setq ido-enable-flex-matching t
      ido-use-filename-at-point t)
;; ido-enter-matching-directory t?
; TODO: What does this do?
;(setq ido-auto-merge-work-directories-length -1)
(add-hook 'ido-setup-hook
          (lambda ()
            (define-key ido-completion-map "\C-p" 'ido-prev-match)
            (define-key ido-completion-map "\C-n" 'ido-next-match)
            (viper-add-local-keys 'insert-state '(("\C-p" . ido-prev-match)
                                                  ("\C-n" . ido-next-match)))
            (define-key ido-completion-map [remap backward-delete-char-untabify] 'ido-delete-backward-updir)))
