(require 'ivy)

;;;###autoload
(defun now-ivy-switch-buffer ()
  "Switch to another buffer."
  (interactive)
  (setq this-command #'ivy-switch-buffer)
  (ivy-read "Switch to buffer: " #'internal-complete-buffer
            :keymap ivy-switch-buffer-map
            :preselect (buffer-name (other-buffer (current-buffer) t))
            :action #'ivy--switch-buffer-action
            :matcher #'ivy--switch-buffer-matcher
            :caller 'ivy-switch-buffer))

(provide 'now-ivy)
