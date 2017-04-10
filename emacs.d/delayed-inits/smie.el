(defun smie-auto-fill (do-auto-fill)
  (if comment-auto-fill-only-comments
      (funcall do-auto-fill)
    (let ((fc (current-fill-column)))
      (when (and fc (> (current-column) fc))
        ;; The loop below presumes BOL is outside of strings or comments.  Also,
        ;; sometimes we prefer to fill the comment than the code around it.
        (unless (or (nth 8 (save-excursion
                             (syntax-ppss (line-beginning-position))))
                    (nth 4 (save-excursion
                             (move-to-column fc)
                             (syntax-ppss))))
          (while
              (and (with-demoted-errors
                       (save-excursion
                         (let ((end (point))
                               (bsf nil)    ;Best-so-far.
                               (gain 0))
                           (beginning-of-line)
                           (while (progn
                                    (smie-indent-forward-token)
                                    (and (<= (point) end)
                                         (<= (current-column) fc)))
                             ;; FIXME?  `smie-indent-calculate' can (and often
                             ;; does) return a result that actually depends on the
                             ;; presence/absence of a newline, so the gain computed
                             ;; here may not be accurate, but in practice it seems
                             ;; to work well enough.
                             (skip-chars-forward " \t")
                             (let* ((newcol (smie-indent-calculate))
                                    (newgain (- (current-column) newcol)))
                               (when (> newgain gain)
                                 (setq gain newgain)
                                 (setq bsf (point)))))
                           (when (> gain 0)
                             (goto-char bsf)
                             (newline-and-indent)
                             'done))))
                   (> (current-column) fc))))
        (when (> (current-column) fc)
          (funcall do-auto-fill))))))
