(icomplete-mode 1)

(defun now-completion-delete-prompt ()
  (set-buffer standard-output)
  (goto-char (point-min))
  (delete-region (point) (search-forward "Possible completions are:\n")))
(add-hook 'completion-setup-hook 'now-completion-delete-prompt 'append)
