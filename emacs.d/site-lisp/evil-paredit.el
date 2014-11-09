(require 'evil)
(require 'paredit)

;;; This is needed, as eval-after-load reports it as possibly missing.
(declare-function eldoc-add-command "eldoc.el")

;;;###autoload (autoload 'evil-paredit-forward-slurp-sexp "evil-paredit" "TBD." t)
(evil-define-command evil-paredit-forward-slurp-sexp (count)
  "TBD."
  :repeat t
  :keep-visual t
  (interactive "p")
  (evil-save-column
    (dotimes (i count)
      (paredit-forward-slurp-sexp))))

;;;###autoload (autoload 'evil-paredit-forward-barf-sexp "evil-paredit" "TBD." t)
(evil-define-command evil-paredit-forward-barf-sexp (count)
  "TBD."
  :repeat t
  :keep-visual t
  (interactive "p")
  (evil-save-column
    (dotimes (i count)
      (paredit-forward-barf-sexp))))

;;;###autoload (autoload 'evil-paredit-backward-slurp-sexp "evil-paredit" "TBD." t)
(evil-define-command evil-paredit-backward-slurp-sexp (count)
  "TBD."
  :repeat t
  :keep-visual t
  (interactive "p")
  (evil-save-column
    (dotimes (i count)
      (paredit-backward-slurp-sexp))))

;;;###autoload (autoload 'evil-paredit-backward-barf-sexp "evil-paredit" "TBD." t)
(evil-define-command evil-paredit-backward-barf-sexp (count)
  "TBD."
  :repeat t
  :keep-visual t
  (interactive "p")
  (evil-save-column
    (dotimes (i count)
      (paredit-backward-barf-sexp))))

;;;###autoload (autoload 'evil-paredit-backward "evil-paredit" "TBD." t)
(evil-define-motion evil-paredit-backward (count)
  "TBD."
  :type exclusive
  (dotimes (i (or count 1))
    (paredit-backward)))

;;;###autoload (autoload 'evil-paredit-backward-up "evil-paredit" "TBD." t)
(evil-define-motion evil-paredit-backward-up (count)
  "TBD."
  :type exclusive
  (dotimes (i (or count 1))
    (paredit-backward-up)))

;;;###autoload (autoload 'evil-paredit-forward "evil-paredit" "TBD." t)
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

;;;###autoload (autoload 'evil-paredit-forward-up "evil-paredit" "TBD." t)
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

(provide 'evil-paredit)
