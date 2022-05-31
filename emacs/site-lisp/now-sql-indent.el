;;; now-sql-indent.el --- sql-indent customization   -*- lexical-binding: t; -*-

;; Copyright © 2022  Nikolai Weibull

;; Author: Nikolai Weibull <now@disu.se>
;; Keywords: local

;;; Code:

(require 'sql-indent)

;;;###autoload
(defun now-sql-indent-init ()
  (setq-default sqlind-basic-offset 8))

(provide 'now-sql-indent)
;;; now-sql-indent.el ends here
