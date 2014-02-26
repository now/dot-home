(eval-when-compile
  (require 'cl))

(defcustom compilation-mode-makefile-name "Makefile"
  "Name of Makefile to look for when using COMPILE-PACKAGE."
  :type 'string
  :group 'compilation)

(defun compile-package-directory (&optional directory makefile-name)
  "Find directory containing a Makefile named MAKEFILE-name, starting at DIRECTORY."
  (let ((directory (or directory default-directory))
        (makefile-name (or makefile-name compilation-mode-makefile-name))
        (root (expand-file-name "/")))
    (or (file-name-directory
         (expand-file-name makefile-name
                           (loop
                            for d = directory then (expand-file-name ".." d)
                            if (file-exists-p (expand-file-name makefile-name d))
                            return d
                            if (equal d root)
                            return nil)))
        directory)))

;;;###autoload
(defun compile-package (&optional directory makefile-name)
  "Compile a package, looking from DIRECTORY upwards for a Makefile named MAKEFILE-NAME."
  (interactive)
  (let ((default-directory (compile-package-directory directory makefile-name)))
    (call-interactively 'compile)))

;;;###autoload
(defun compile-package-immediately (&optional command directory makefile-name)
  "Compile a package immediately, looking from DIRECTORY upwards for a Makefile named MAKEFILE-NAME."
  (interactive)
  (let ((default-directory (compile-package-directory directory makefile-name)))
    (compile (or command compile-command))))

(provide 'compile-package)
