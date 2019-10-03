(require 'cc-mode)

;;;###autoload
(defun now-c-fill-paragraph (&optional arg)
  "Like `c-fill-paragraph', but add indent to `fill-column'.
The `fill-column' will be set to its current value, plus any
indent, if we're filling a paragraph, though not any higher than
79."
  (interactive "*P")
  (let ((c-lit-limits (c-literal-limits nil t)))
    (when (memq (c-literal-type c-lit-limits) '(c c++))
      (let ((fill-column (min (+ (current-fill-column)
                                 (- (car c-lit-limits) (point-at-bol)))
                              79)))
        (c-fill-paragraph arg)))))

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

(defun now-c-mode-prettify-symbols-compose-p (start end match)
  "Return true iff the symbol MATCH should be composed in C mode.
The symbol starts at position START and ends at position END."
  (if (save-match-data (string-match "^\\(?:\\[.*\\]\\|--\\)$" match))
      t
    (let* ((syntaxes-beg (if (memq (char-syntax (char-after start)) '(?w ?_))
                             '(?w ?_) '(?. ?\\)))
           (syntaxes-end (if (memq (char-syntax (char-before end)) '(?w ?_))
                             '(?w ?_) '(?. ?\\))))
      (not (or (memq (char-syntax (or (char-before start) ?\s)) syntaxes-beg)
               (memq (char-syntax (or (char-after end) ?\s)) syntaxes-end)
               (nth 8 (syntax-ppss)))))))

(provide 'now-cc-mode)
