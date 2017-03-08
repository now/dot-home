(require 'gxref)

;;;###autoload
(defun now-gxref-find-file ()
  "Switch to a file in a GTAGS project via `ido' completion."
  (interactive)
  (let ((default-directory (gxref--find-project-root)))
    (unless default-directory (error "Not under a GTAGS project"))
    (let* ((minibuffer-completing-file-name t)
           (filename (ido-completing-read "Find file: "
                                          (gxref--global-to-list '("-P")) nil
                                          (confirm-nonexistent-file-or-buffer)
                                          nil 'ido-file-history)))
      (when filename
        (find-file filename)))))
