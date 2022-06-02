;;; now-lisp-mode.el --- lisp-mode customizations    -*- lexical-binding: t; -*-

;; Copyright © 2022  Nikolai Weibull

;; Author: Nikolai Weibull <now@disu.se>
;; Keywords: local

;;; Code:

;;;###autoload
(defun now-lisp-mode-init ()
  (define-keymap
    :keymap lisp-mode-shared-map
    "C-c r" 'raise-sexp
    "C-c s" 'delete-pair))

(provide 'now-lisp-mode)
;;; now-lisp-mode.el ends here
