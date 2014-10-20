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
     (defun evil-paredit-enter-and-exit-motion-state-to-set-keys ()
       (evil-motion-state)
       (evil-normal-state))
     (add-hook 'paredit-mode-hook 'evil-paredit-enter-and-exit-motion-state-to-set-keys)
     (evil-define-key 'motion paredit-mode-map
       "gB" 'evil-paredit-backward-up
       "gb" 'evil-paredit-backward
       "gW" 'evil-paredit-forward-up
       "gw" 'evil-paredit-forward)
     (evil-define-key 'normal paredit-mode-map
       "D" 'paredit-kill
       "x" 'paredit-forward-delete
       "X" 'paredit-backward-delete
       "))" 'evil-paredit-forward-slurp-sexp
       ")}" 'evil-paredit-forward-barf-sexp
       "((" 'evil-paredit-backward-slurp-sexp
       "({" 'evil-paredit-backward-barf-sexp
       "(J" 'paredit-join-sexps
       "(R" 'paredit-raise-sexp
       "(S" 'paredit-splice-sexp-killing-backward
       "(s" 'paredit-splice-sexp
       "(W" 'paredit-wrap-round)))