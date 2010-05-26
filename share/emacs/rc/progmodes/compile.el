(defun compilation-finish-autoclose (buffer string)
  (when (not (or compilation-current-error
                 (string-match-p "exited-normally" string)))
    (bury-buffer)
    ;; TODO: We should really be able to tell how many windows there were
    ;; before we begin compiling.  If there are two, this should be
    ;; replace-buffer-in-windows instead.
    (delete-window (get-buffer-window buffer t))))

(setq compilation-auto-jump-to-first-error t
      compilation-scroll-output t
      compilation-finish-functions 'compilation-finish-autoclose)
