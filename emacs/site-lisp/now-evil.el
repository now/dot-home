;;;###autoload
(defun now-evil-delete-auto-indent ()
  "Delete any automatically inserted indent.
This function checks if `last-command' is one of `evil-ret',
`c-context-line-break', or `newline' and calls
`delete-horizontal-space if so.  It also checks for
`c-electric-brace', `c-electric-colon', and
`c-electric-semi&comma' and calls `delete-horizontal-space'
followed by `delete-char' to remove the newline as well."
  (when (eolp)
    (pcase last-command
      ((or 'evil-ret 'c-context-line-break 'newline) (delete-horizontal-space))
      ((or 'c-electric-brace 'c-electric-colon 'c-electric-semi&comma)
       (delete-horizontal-space)
       (when (bolp)
         (delete-char -1))))))

(provide 'evil)
