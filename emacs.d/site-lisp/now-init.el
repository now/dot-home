;;;###autoload
(defun now-comment-auto-fill-only-comments ()
  "Set `comment-auto-fill-only-comments' to t."
  (setq comment-auto-fill-only-comments t))

;;;###autoload
(defun now-do-not-show-trailing-whitespace ()
  "Set `show-trailing-whitespace' to 'nil."
  (setq show-trailing-whitespace nil))

;;;###autoload
(defun now-set-fill-column-to-79 ()
  "Set `fill-column' to 79."
  (setq fill-column 79))

(provide 'now-init)
