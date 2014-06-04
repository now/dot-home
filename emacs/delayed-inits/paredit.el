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
     (evil-define-key 'normal paredit-mode-map
       "D" 'paredit-kill
       "gb" 'paredit-backward
       "gB" 'paredit-forward-up
       "gW" 'paredit-backward-up
       "gw" 'evil-paredit-forward
       "x" 'paredit-forward-delete
       "X" 'paredit-backward-delete
       "))" 'evil-paredit-forward-slurp-sexp
       ")}" 'evil-paredit-forward-barf-sexp
       "((" 'evil-paredit-backward-slurp-sexp
       "({" 'evil-paredit-backward-barf-sexp
       "(J" 'paredit-join-sexps
       "(R" 'paredit-raise-sexp
       "(S" 'paredit-split-sexp
       "(s" 'paredit-splice-sexp
       "(W" 'paredit-wrap-sexp)))
