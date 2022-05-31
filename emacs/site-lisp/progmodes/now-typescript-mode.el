;;; now-typescript-mode.el --- typescript-mode customizations  -*- lexical-binding: t; -*-

;; Copyright © 2022  Nikolai Weibull

;; Author: Nikolai Weibull <now@disu.se>
;; Keywords: local

;;; Code:

(require 'compile)

;;;###autoload
(defun now-typescript-mode-init ()
  (dolist (entry '(typescript-tsc
                   typescript-tsc-pretty
                   typescript-tslint
                   typescript-nglint-error
                   typescript-nglint-warning))
    (setf (alist-get entry compilation-error-regexp-alist nil 'remove) nil)))

(provide 'now-typescript-mode)
;;; now-typescript-mode.el ends here
