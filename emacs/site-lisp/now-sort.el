;;; now-sort.el --- Sort functions                   -*- lexical-binding: t; -*-

;;; Code:

;;;###autoload
(defun sort-words (reverse beg end)
  "Sort words in region alphabetically, in REVERSE if negative.
Prefixed with negative \\[universal-argument], sorts in reverse.

The variable `sort-fold-case' determines whether alphabetic case
affects the sort order.

See `sort-regexp-fields'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\w+" "\\&" beg end))

(provide 'now-sort)
;;; now-sort.el ends here
