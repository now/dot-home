(defcustom project-filename "Makefile"
  "Name of file to look for when looking for project root."
  :type 'string
  :group 'compilation)

(defun project-directory (&optional directory filename)
  "Find directory containing a file named FILENAME, starting at DIRECTORY."
  (let ((directory (or directory default-directory))
        (filename (or filename project-filename)))
    (or (locate-dominating-file directory filename)
        directory)))

(defmacro with-project-directory (directory filename &rest body)
  `(let ((default-directory (project-directory ,directory ,filename)))
    ,@body))

;;;###autoload
(defun compile-project (&optional directory filename)
  "Compile a project, looking from DIRECTORY upwards for a file named FILENAME.

DIRECTORY defaults to \(default-directory).
FILENAME defaults to \(project-filename)."
  (interactive)
  (with-project-directory directory filename
                          (call-interactively 'compile)))

;;;###autoload
(defun compile-project-immediately (&optional command directory makefile-name)
  "Compile a project immediately, looking from DIRECTORY upwards for a file named FILENAME.

DIRECTORY defaults to \(default-directory).
FILENAME defaults to \(project-filename)."
  (interactive)
  (with-project-directory directory makefile-name
                          (compile (or command compile-command))))

;;;###autoload
(defun find-project-file ()
  "Find a file, starting at the project root."
  (interactive)
  (with-project-directory nil nil
                          (call-interactively 'find-file)))

;;;###autoload
(defun project-shell-command ()
  "Run SHELL-COMMAND with DEFAULT-DIRECTORY set to the project root."
  (interactive)
  (with-project-directory nil nil (call-interactively 'shell-command)))

(provide 'project)
