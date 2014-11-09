(require 'evil)
(require 'paredit)

;;; This is needed, as eval-after-load reports it as possibly missing.
(declare-function eldoc-add-command "eldoc.el")

;;;###autoload
(evil-define-command evil-paredit-forward-slurp-sexp (count)
  "TBD."
  :repeat t
  :keep-visual t
  (interactive "p")
  (evil-save-column
    (dotimes (i count)
      (paredit-forward-slurp-sexp))))

;;;###autoload
(evil-define-command evil-paredit-forward-barf-sexp (count)
  "TBD."
  :repeat t
  :keep-visual t
  (interactive "p")
  (evil-save-column
    (dotimes (i count)
      (paredit-forward-barf-sexp))))

;;;###autoload
(evil-define-command evil-paredit-backward-slurp-sexp (count)
  "TBD."
  :repeat t
  :keep-visual t
  (interactive "p")
  (evil-save-column
    (dotimes (i count)
      (paredit-backward-slurp-sexp))))

;;;###autoload
(evil-define-command evil-paredit-backward-barf-sexp (count)
  "TBD."
  :repeat t
  :keep-visual t
  (interactive "p")
  (evil-save-column
    (dotimes (i count)
      (paredit-backward-barf-sexp))))

;;;###autoload
(evil-define-motion evil-paredit-backward (count)
  "TBD."
  :type exclusive
  (dotimes (i (or count 1))
    (paredit-backward)))

;;;###autoload
(evil-define-motion evil-paredit-backward-up (count)
  "TBD."
  :type exclusive
  (dotimes (i (or count 1))
    (paredit-backward-up)))

;;;###autoload
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

;;;###autoload
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
