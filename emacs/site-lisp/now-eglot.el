;;; now-eglot.el --- eglot configuration             -*- lexical-binding: t; -*-

(require 'eglot)

;;;###autoload
(defun now-eglot-format-managed-buffer ()
  (when (eglot-managed-p)
    (eglot-format-buffer)))

(provide 'now-eglot)

;;; now-eglot.el ends here
