;;; now-disp-table.el --- disp-table customization   -*- lexical-binding: t; -*-

;; Copyright © 2022  Nikolai Weibull

;; Author: Nikolai Weibull <now@disu.se>
;; Keywords: local

;;; Code:

(defface wrap-glyph
  '((((min-colors 16) (class color)) :foreground "blue")
    (t :inverse-video t))
  "Face for wrap glyph."
  :group 'basic-faces)

;;;###autoload
(defun now-disp-table-init ()
  (set-display-table-slot standard-display-table 'wrap #x00a0)
  (set-display-table-slot
   standard-display-table 'selective-display (vector (make-glyph-code #x2026)))
  (set-display-table-slot standard-display-table 'vertical-border 0))

(provide 'now-disp-table)
;;; now-disp-table.el ends here
