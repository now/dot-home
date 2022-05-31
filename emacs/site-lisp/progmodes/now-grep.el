;;; now-grep.el --- grep customizations              -*- lexical-binding: t; -*-

;; Copyright © 2022  Nikolai Weibull

;; Author: Nikolai Weibull <now@disu.se>
;; Keywords: local

;;; Code:

(require 'grep)

;;;###autoload
(defun now-grep-init ()
  "Initialize `grep' customizations."
  (setq
   grep-regexp-alist
   `((,(rx-let ((null-file (group-n 1 (one-or-more (not (in ?\0 ?\n)))))
                (nul-after-file (group-n 3 ?\0))
                (null-line (group-n 2 (one-or-more digit)))
                (nul-after-line (group-n 4 ?\0))
                (:-or-nul-after-line (or ?: nul-after-line))
                (null
                 (sequence null-file nul-after-file null-line :-or-nul-after-line))
                (optional-drive (optional (in (?a . ?z) (?A . ?Z)) ?:))
                (not-null-file-part
                 (minimal-match (one-or-more (not (in ?\n ?:)))))
                (not-null-file-end (not (in ?\n ?: ?/)))
                (not-null-file
                 (group-n 1 optional-drive not-null-file-part not-null-file-end))
                (not-null-line (group-n 2 (in (?1 . ?9)) (zero-or-more digit)))
                (not-null (sequence not-null-file ?: not-null-line ?:)))
         (rx (sequence line-start (or null not-null))))
      1
      2
      (now-grep-column . now-grep-end-column)
      nil
      nil
      (3 '(face nil display ":"))
      (4 '(face nil display ":")))
     ("^Binary file \\(.+\\) matches$" 1 nil nil 0 1))))

(defun now-grep-column (&optional want-end)
  "Return start column of `grep' match."
  (when grep-highlight-matches
    (let* ((start (match-end 0))
           (end (save-excursion (goto-char start) (line-end-position)))
           (match-start
            (text-property-any start end 'font-lock-face grep-match-face))
           (match-end
            (when match-start
              (next-single-property-change
               match-start 'font-lock-face nil end)))
           (column (if want-end match-end match-start)))
      (when column
        (- column start)))))

(defun now-grep-end-column ()
  "Return end column of `grep' match."
  (now-grep-column t))

(provide 'now-grep)
;;; now-grep.el ends here
