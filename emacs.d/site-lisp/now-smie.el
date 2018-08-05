;;;###autoload
(defun now-smie-auto-fill (f do-auto-fill)
  (if comment-auto-fill-only-comments
      (funcall do-auto-fill)
    (apply f do-auto-fill)))

(provide 'now-smie)
