(eval-after-load 'evil
  '(progn
     (evil-define-command evil-paredit-forward-slurp-sexp (count)
       "TBD."
       :repeat t
       :keep-visual t
       (interactive "p")
       (evil-save-column
         (dotimes (i count)
           (paredit-forward-slurp-sexp))))
     (evil-define-command evil-paredit-forward-barf-sexp (count)
       "TBD."
       :repeat t
       :keep-visual t
       (interactive "p")
       (evil-save-column
         (dotimes (i count)
           (paredit-forward-barf-sexp))))
     (evil-define-command evil-paredit-backward-slurp-sexp (count)
       "TBD."
       :repeat t
       :keep-visual t
       (interactive "p")
       (evil-save-column
         (dotimes (i count)
           (paredit-backward-slurp-sexp))))
     (evil-define-command evil-paredit-backward-barf-sexp (count)
       "TBD."
       :repeat t
       :keep-visual t
       (interactive "p")
       (evil-save-column
         (dotimes (i count)
           (paredit-backward-barf-sexp))))
     (evil-define-motion evil-paredit-backward (count)
       "TBD."
       :type exclusive
       (dotimes (i (or count 1))
         (paredit-backward)))
     (evil-define-motion evil-paredit-backward-up (count)
       "TBD."
       :type exclusive
       (dotimes (i (or count 1))
         (paredit-backward-up)))
     (evil-define-motion evil-paredit-forward (count)
       "TBD."
       :type exclusive
       (if (and evil-move-cursor-back
                (eq last-command 'evil-paredit-forward)
                (= (or count 1) 1)
                (not (eolp)) (save-excursion (forward-char) (eolp)))
         (forward-char))
       (dotimes (i (or count 1))
         (paredit-forward)))
     (evil-define-motion evil-paredit-forward-up (count)
       "TBD."
       :type exclusive
       (if (and evil-move-cursor-back
                (eq last-command 'evil-paredit-forward-up)
                (= (or count 1) 1)
                (not (eolp)) (save-excursion (forward-char) (eolp)))
         (forward-char))
       (dotimes (i (or count 1))
         (paredit-forward-up)))
     (add-hook 'paredit-mode-hook
               (lambda ()
                 (define-key evil-motion-state-local-map "gB" 'evil-paredit-backward-up)
                 (define-key evil-motion-state-local-map "gb" 'evil-paredit-backward)
                 (define-key evil-motion-state-local-map "gW" 'evil-paredit-forward-up)
                 (define-key evil-normal-state-local-map "gw" 'evil-paredit-forward)
                 (define-key evil-normal-state-local-map "D" 'paredit-kill)
                 (define-key evil-normal-state-local-map "x" 'paredit-forward-delete)
                 (define-key evil-normal-state-local-map "X" 'paredit-backward-delete)
                 (define-key evil-normal-state-local-map "))" 'evil-paredit-forward-slurp-sexp)
                 (define-key evil-normal-state-local-map ")}" 'evil-paredit-forward-barf-sexp)
                 (define-key evil-normal-state-local-map "((" 'evil-paredit-backward-slurp-sexp)
                 (define-key evil-normal-state-local-map "({" 'evil-paredit-backward-barf-sexp)
                 (define-key evil-normal-state-local-map "(J" 'paredit-join-sexps)
                 (define-key evil-normal-state-local-map "(R" 'paredit-raise-sexp)
                 (define-key evil-normal-state-local-map "(S" 'paredit-splice-sexp-killing-backward)
                 (define-key evil-normal-state-local-map "(s" 'paredit-splice-sexp)
                 (define-key evil-normal-state-local-map "(W" 'paredit-wrap-round)))))
