;;; now-isearch.el --- isearch customizations        -*- lexical-binding: t; -*-

;; Copyright © 2022  Nikolai Weibull

;; Author: Nikolai Weibull <now@disu.se>
;; Keywords: local

;;; Code:

;;;###autoload
(defun now-isearch-init ()
  (keymap-set isearch-mode-map "C-'" 'avy-isearch))

(provide 'now-isearch)
;;; now-isearch.el ends here
