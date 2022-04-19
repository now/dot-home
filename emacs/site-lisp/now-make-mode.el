;;; now-make-mode.el --- make-mode customization     -*- lexical-binding: t; -*-

(require 'whitespace)

;;;###autoload
(defun now-make-mode-remove-space-after-tab-from-whitespace-style ()
  (setq-local whitespace-style (cl-remove 'space-after-tab whitespace-style)))

(provide 'now-make-mode)
