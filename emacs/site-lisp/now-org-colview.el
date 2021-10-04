(require 'org)

;;;###autoload
(defun now-org-columns-forward-char ()
  "Go to the next column field in the current row."
  (interactive)
  (goto-char (1+ (point))))

;;;###autoload
(eval-when-compile
  (declare-function org-agenda-do-context-action "org-agenda.el"))
(defun now-org-columns-forward-line (n)
  (interactive "p")
  (let ((col (current-column))
        (m (if (< 0 n) 2 0)))
    (beginning-of-line m)
    (while (and (org-invisible-p2) (not (if (< 0 n) (bobp) (eobp))))
      (beginning-of-line m))
    (move-to-column col)
    (if (eq major-mode 'org-agenda-mode)
        (org-agenda-do-context-action))))

;;;###autoload
(defun now-org-columns-backward-line (n)
  (interactive "p")
  (now-org-columns-forward-line (- n)))

(provide 'now-org-colview)
