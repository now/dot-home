(require 'cc-mode)

;;;###autoload
(defun now-c-mode-adaptive-fill-function ()
  (when (looking-at "\\([ \t]*\\(//+\\|\\**\\)[ \t]*\\)\\(?:@[[:alpha:]]+\\|•\\)[ \t]")
    (concat (match-string 1) "  ")))

;;;###autoload
(defun now-c-auto-newline-mode (&optional arg)
  "Toggle auto-newline mode on or off.
With a prefix argument ARG, enable auto-newline mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil, and toggle it if ARG is `toggle'.

Turning on auto-newline mode automatically enables electric
indentation.

When auto-newline mode is on (indicated by “/la” on the mode
line after the mode name) newlines are automatically inserted
after special characters such as brace, comma, semi-colon, and
colon."
  (interactive (list (or current-prefix-arg 'toggle)))
  (c-toggle-auto-newline (if (eq arg 'toggle)
                             (not (and c-auto-newline c-electric-flag))
                           (> (prefix-numeric-value arg) 0))))

(provide 'now-cc-mode)
