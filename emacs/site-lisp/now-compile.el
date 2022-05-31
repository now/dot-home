;;; now-compile.el --- compilation customizations    -*- lexical-binding: t; -*-

;; Copyright © 2022  Nikolai Weibull

;; Author: Nikolai Weibull <now@disu.se>
;; Keywords: local

;;; Code:

(require 'compile)

;;;###autoload
(defun now-compile-init ()
  "Initialize customizations to `compile'."
  (rx-let ((spaces (one-or-more ?\s))
           (not-space-or-colon (not (in ?\n ?\s ?:)))
           (space-not---or-/ (sequence ?\s (not (in ?- ?/ ?\n))))
           (colon-not-space (sequence ?: (not (in ?\s ?\n))))
           (file-init (sequence (zero-or-more digit) (not (in digit ?\n))))
           (file-part (or not-space-or-colon space-not---or-/ colon-not-space))
           (file-parts (minimal-match (zero-or-more file-part)))
           (file (group-n 1 file-init file-parts))
           (line (group-n 2 (one-or-more digit)))
           (column (group-n 3 (one-or-more digit)))
           (java-line-column-colons (sequence line (optional ?: column) ?:))
           (java-line-column-brackets (sequence ?\[ line (optional ?, column) ?\]))
           (java-line-column (or java-line-column-colons java-line-column-brackets))
           (maven-error "ERROR")
           (maven-warning (group-n 4 (sequence "WARN" (optional "ING"))))
           (maven-info (group-n 5 "INFO"))
           (maven-type* (or maven-error maven-warning maven-info))
           (maven-type-junk (optional ?\] spaces ?\[ (or "Warn" "Error")))
           (maven-type (sequence maven-type* maven-type-junk))
           (sbt-error "error")
           (sbt-warning (group-n 4 (: "warn" (? "ing"))))
           (sbt-info (group-n 5 "info"))
           (sbt-type (or sbt-error sbt-warning sbt-info)))
    (dolist (cons
             `((maven
                ,(rx line-start ?\[ maven-type ?\] spaces file ?: java-line-column ?\s)
                now-compilation-maven-file
                2
                3
                (4 . 5)
                nil
                (1 (funcall 'now-compilation-maven-highlight)))
               (sbt
                ,(rx line-start ?\[ sbt-type ?\] spaces file ?: java-line-column ?\s)
                1
                2
                3
                (4 . 5))
               (typescript-X
                ,(rx line-start file "(" line "," column "): ")
                1
                2
                3)
               (typescript ;; webkit?
                ,(rx line-start file "\n  Line " line ":" column ":  ")
                1
                2
                3)
               (typescript-following
                ,(rx line-start "  Line " line ":" column ":  ")
                nil
                2
                3)))
      (setf (alist-get (car cons) compilation-error-regexp-alist-alist)
            (cdr cons)))))

(defun now-compilation-maven-file ()
  (declare-function compilation--previous-directory "compile")
  (let ((s (match-string-no-properties 1)))
    (if (or (null s) (file-name-absolute-p s))
        s
      (let* ((pos (compilation--previous-directory
                   (match-beginning 0)))
             (dir (when pos
                    (or (get-text-property (1- pos) 'compilation-directory)
                        (get-text-property pos 'compilation-directory))))
             (dir (when dir (file-name-as-directory (car dir)))))
        (if dir
            (if (file-exists-p (concat dir s))
                s
              (let* ((s (replace-regexp-in-string "\\$.*" "" s))
                     (java-1 (format "%s.java" s))
                     (scala-1 (format "%s.scala" s))
                     (java-2 (format "%s.java"
                                     (replace-regexp-in-string "\\." "/" s)))
                     (scala-2 (format "%s.scala"
                                      (replace-regexp-in-string "\\." "/" s))))
                (cl-find-if
                 (lambda (f) (file-exists-p (concat dir f)))
                 (list
                  java-1
                  scala-1
                  java-2
                  scala-2
                  (format "src/main/java/%s" java-1)
                  (format "src/main/scala/%s" scala-1)
                  (format "src/test/java/%s" java-1)
                  (format "src/test/scala/%s" scala-1)
                  (format "src/main/java/%s" java-2)
                  (format "src/main/scala/%s" scala-2)
                  (format "src/test/java/%s" java-2)
                  (format "src/test/scala/%s" scala-2)))))
          s)))))

;;;###autoload
(defun now-compilation-maven-highlight ()
  (let ((type (compilation-type '(4 . 5))))
    (compilation--note-type type)
    (symbol-value (aref [compilation-info-face
                         compilation-warning-face
                         compilation-error-face]
                        type))))

(provide 'now-compile)
;;; now-compile.el ends here
