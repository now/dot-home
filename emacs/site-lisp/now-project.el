(require 'project)

;;;###autoload
(defun now-project-display-compilation ()
  "If a *compilation* buffer exists, ‘display-buffer’ it.
Otherwise, start a new ‘compile’."
  (interactive)
  (let ((b (cl-find-if (lambda (b) (equal (buffer-name b) "*compilation*"))
		       (buffer-list))))
    (when b
      (display-buffer b))
    (call-interactively #'project-compile)))

(provide 'now-project)
