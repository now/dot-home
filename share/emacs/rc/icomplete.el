(icomplete-mode 1)
(setq completion-show-help nil)
;; TODO: Don’t know about this one.
(defun my-completion-delete-prompt ()
  (set-buffer standard-output)
  (goto-char (point-min))
  (delete-region (point) (search-forward "Possible completions are:\n")))
(add-hook 'completion-setup-hook 'my-completion-delete-prompt 'append)
;; TODO: Look into partial-completion-mode.
(partial-completion-mode 1)
(setq completions-format 'vertical)
