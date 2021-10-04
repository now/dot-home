(defvar hide-mode-line--saved-mode-line-format nil
  "Saved `mode-line-format' used by
`hide-mode-line-show-mode-line'.")
(make-variable-buffer-local 'hide-mode-line--saved-mode-line-format)

(defun hide-mode-line--pre-redisplay-function (window)
  "Clear `mode-line-format` for `current-buffer'.
The current value of `mode-line-format' is saved in
`hide-mode-line-mode--saved-mode-line-format' before it’s cleared
so that it can be displayed by `hide-mode-line-show-mode-line',
unless it’s already `nil'."
  (when mode-line-format
    (setq hide-mode-line--saved-mode-line-format mode-line-format))
  (setq mode-line-format nil))

;;;###autoload
(defun hide-mode-line-show-mode-line ()
  "Display the saved mode line at the bottom of the screen.
If there’s no saved mode line to display using `message' in
`hide-mode-line-saved-mode-line-format', use `mode-line-format'."
  (interactive)
  (message "%s" (format-mode-line (or hide-mode-line--saved-mode-line-format
                                      mode-line-format))))

;;;###autoload
(define-minor-mode hide-mode-line-mode
  "Hide the mode line of all buffers.
Use `hide-mode-line-show-mode-line' to show the mode line of
the current buffer temporarily at the bottom of the screen."
  :group 'mode-line
  :global t
  (apply (if hide-mode-line-mode 'add-hook 'remove-hook)
      'pre-redisplay-functions
      '(hide-mode-line--pre-redisplay-function))
  (unless hide-mode-line-mode
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when hide-mode-line--saved-mode-line-format
          (setq mode-line-format hide-mode-line--saved-mode-line-format))
        (kill-local-variable 'hide-mode-line--saved-mode-line-format)))))

(provide 'hide-mode-line)
