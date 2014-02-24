(eval-after-load 'evil
  '(evil-define-key 'normal paredit-mode-map
     "D" 'paredit-kill
     "gh" 'paredit-backward
     "gj" 'paredit-forward-up
     "gk" 'paredit-backward-up
     "gs" 'paredit-forward
     "x" 'paredit-forward-delete
     "X" 'paredit-backward-delete
     "))" 'paredit-forward-slurp-sexp
     ")}" 'paredit-forward-barf-sexp
     "((" 'paredit-backward-slurp-sexp
     "({" 'paredit-backward-barf-sexp
     "(J" 'paredit-join-sexps
     "(R" 'paredit-raise-sexp
     "(S" 'paredit-split-sexp
     "(s" 'paredit-splice-sexp
     "(W" 'paredit-wrap-sexp))
