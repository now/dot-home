;;; now-eglot.el --- eglot configuration             -*- lexical-binding: t; -*-

(require 'eglot)

;;;###autoload
(defun now-eglot-format-managed-buffer ()
  (when (eglot-managed-p)
    (eglot-format-buffer)))

;;;###autoload
(defun now-eglot-init ()
  (setq
   eglot-server-programs
   `((java-mode
      .
      ("jdtls"
       "-configuration"
       ,(concat
         (file-name-as-directory
          (or (getenv "XDG_CONFIG_CACHE") (expand-file-name "~/.cache")))
         "jdtls")
       "-data"
       ,(expand-file-name "~/Projects/eclipse-data"))))))

(provide 'now-eglot)

;;; now-eglot.el ends here
