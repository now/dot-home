;;; now-elisp-mode.el --- elisp-mode customizations  -*- lexical-binding: t; -*-

;; Copyright © 2022  Nikolai Weibull

;; Author: Nikolai Weibull <now@disu.se>
;; Keywords: local

;;; Code:

;;;###autoload
(defun now-elisp-mode-init ()
  (font-lock-add-keywords
   'emacs-lisp-mode
   `((,(rx
        ?\(
        (group
         (or "cl-assert" "cl-check-type" "error" "signal" "user-error" "warn"))
        symbol-end)
      (1 font-lock-keyword-face)))))

(provide 'now-elisp-mode)
;;; now-elisp-mode.el ends here
