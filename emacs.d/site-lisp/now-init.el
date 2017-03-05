;;;###autoload
(defun now-comment-auto-fill-only-comments ()
  "Set `comment-auto-fill-only-comments' to t."
  (setq comment-auto-fill-only-comments t))

;;;###autoload
(defun now-show-trailing-whitespace ()
  "Set `show-trailing-whitespace' to t."
  (setq show-trailing-whitespace t))

;;;###autoload
(defun now-set-fill-column-to-79 ()
  "Set `fill-column' to 79."
  (setq fill-column 79))

;;;###autoload
(defun now-turn-on-whitespace-mode ()
  "Turn on `whitespace-mode' with reasonable `whitespace-style'."
  (set (make-local-variable 'whitespace-style)
       '(face trailing lines-tail empty indentation space-before-tab))
  (set (make-local-variable 'whitespace-line-column) 81)
  (whitespace-mode 1))

(provide 'now-init)
