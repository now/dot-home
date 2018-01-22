;;;###autoload
(defun now-comment-auto-fill-only-comments ()
  "Set `comment-auto-fill-only-comments' to t."
  (setq comment-auto-fill-only-comments t))

;;;###autoload
(defun now-set-fill-column-to-79 ()
  "Set `fill-column' to 79."
  (setq fill-column 79))

;;;###autoload
(defun now-turn-on-whitespace-mode ()
  "Turn on `whitespace-mode' with reasonable `whitespace-style'."
  (whitespace-mode))

(provide 'now-init)
