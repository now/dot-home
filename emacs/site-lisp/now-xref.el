;;; now-xref.el --- xref customizations              -*- lexical-binding: t; -*-

;; Copyright © 2022  Nikolai Weibull

;; Author: Nikolai Weibull <now@Nikolais-MacBook-Pro-2.local>
;; Keywords: local

;;; Code:

(require 'xref)

;;;###autoload
(defun now-xref-init ()
  (keymap-set xref--xref-buffer-mode-map "q" 'quit-window))

(provide 'now-xref)
;;; now-xref.el ends here
