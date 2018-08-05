;;;###autoload
(defun now-comment-auto-fill-only-comments ()
  "Set `comment-auto-fill-only-comments' to t."
  (setq comment-auto-fill-only-comments t))

;;;###autoload
(defun now-set-fill-column-to-79 ()
  "Set `fill-column' to 79."
  (setq fill-column 79))

;;;###autoload
(defun now-tabulated-list-mode-use-global-glyphless-char-display ()
  "Kill the local variable `glyphless-char-display'."
  (kill-local-variable 'glyphless-char-display))

;;;###autoload
(defun now-remove-continuation-fringe-indicator ()
  "Remove `'continuation' from `fringe-indicator-alist'."
  (setf (alist-get 'continuation fringe-indicator-alist) nil))

;;;###autoload
(defun now-remove-truncation-fringe-indicator ()
  "Remove `'continuation' from `fringe-indicator-alist'."
  (setf (alist-get 'truncation fringe-indicator-alist) nil))

(provide 'now-init)
