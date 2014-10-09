(defcustom project-filename (purecopy  "Makefile")
  "Name of file to look for when looking for project root."
  :group 'compilation
  :type 'string)

(defcustom auto-compile-project-command (purecopy "make ")
  "Command Auto-Compile-Project Mode."
  :group 'compilation
  :type 'string)

(defvar auto-compile-project-timer nil
  "Timer used by `auto-compile-project-set-timer'.")

(defcustom auto-compile-project-timeout 5
  "Timeout, in seconds, after which `compile-project' will be
called automatically when Auto-Compile-Project Mode is enabled.
The alue may be an integer or floating point number.

If a timer is already active, there are two ways to make sure
that the new value will take effect immediately.  You can set
this variable through Custom or you can call the command
`auto-compile-project-set-timer' after setting the variable.
Otherwise, the new value will take effect the first time Auto
Compile Project Mode calls `auto-compile-project-set-timer' for
internal reasons or in your next editing session."
  :group 'compilation
  :type 'number
  :set (lambda (variable value)
	 (set-default variable value)
	 (and (boundp 'auto-compile-project-timer)
	      auto-compile-project-timer
	      (auto-compile-project-set-timer))))

(defcustom auto-compile-project-mode-text " ACP"
  "String to display in the mode line when Auto-Compile-Project Mode is active.

\(When the string is not empty, make sure that it has a leading space.)"
  :group 'compilation
  :type 'string)

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

;;;###autoload
(define-minor-mode auto-compile-project-mode
  "Toggle automatic project compilation when a file is saved.
With a prefix argument ARG, enable Auto Project Compilation mode
if ARG is positive, and disable it otherwise.  If called from
Lisp, enable the mode if ARG is omitted or nil.

Auto Project Compilation mode is a minor mode that affects only
the current buffer.  When enabled, it compiles the project the
file the buffer is visiting belongs to when the buffer is saved."
  :group 'compilation
  :lighter auto-compile-project-mode-text
  (if (not auto-compile-project-mode)
      (remove-hook 'after-save-hook 'auto-compile-project-set-timer t)
    (make-local-variable 'auto-compile-project-timer)
    (add-hook 'after-save-hook 'auto-compile-project-set-timer nil t)))

;;;###autoload
(defun turn-on-auto-compile-project-mode ()
  "Turn on Auto-Compile-Project Mode.

This function is designed to be added to hooks, for example:
  (add-hook 'org-mode-hook 'turn-on-auto-compile-project-mode)"
  (auto-compile-project-mode 1))

(defun auto-compile-project-set-timer ()
  "Run `compile-project' after `auto-compile-project-timeout' seconds of idle time.

Any previouly scheduled compilation is first removed from the
schedule."
  (if (timerp auto-compile-project-timer)
      (cancel-timer auto-compile-project-timer))
  (setq auto-compile-project-timer
        (run-with-idle-timer auto-compile-project-timeout nil
                             'compile-project-immediately auto-compile-project-command)))

(provide 'project)
