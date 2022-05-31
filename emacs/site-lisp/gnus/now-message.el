;;; now-message.el --- message customizations        -*- lexical-binding: t; -*-

;; Copyright © 2022  Nikolai Weibull

;; Author: Nikolai Weibull <now@disu.se>
;; Keywords: local

;;; Code:

(require 'message)

;;;###autoload
(defun now-message-init ()
  (define-abbrev message-mode-abbrev-table "br"
    "Best regards,\n  Nikolai"
    nil
    :system t
    :case-fixed t)
  (define-abbrev message-mode-abbrev-table "tbr"
    "Thank you and best regards,\n  Nikolai"
    nil
    :system t
    :case-fixed t)
  (define-abbrev message-mode-abbrev-table "tsn"
    "Thanks,\n  Nikolai"
    nil
    :system t
    :case-fixed t))

(provide 'now-message)
;;; now-message.el ends here
