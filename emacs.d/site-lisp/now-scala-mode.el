;; now-scala-mode.el --- Scala-mode configuration  -*- lexical-binding: t -*-

(require 'now-cc-mode)
(require 'scala-mode)
(require 'skeleton)

;;;###autoload
(defun now-scala-adaptive-fill-function ()
  "Function to use for ‘scala-mode’ ‘adaptive-fill-function’.
This function first tries ‘now-c-mode-adaptive-fill-function’,
then ‘scala-paragraph:fill-function’."
  (or (now-c-mode-adaptive-fill-function)
      (scala-paragraph:fill-function)))

;;;###autoload
(define-skeleton now-scala-mode-auto-insert-mode-skeleton
  "Skeleton for ‘auto-insert-mode’ in ‘scala-mode’ buffers.
Insert a package definition based on the ‘buffer-file-name’."
              nil
              "package " (replace-regexp-in-string
                          "/" "."
                          (replace-regexp-in-string
                           ".*/src/\\(?:main\\|test\\)/[^/]+/\\(.*\\)/$" "\\1"
                           (file-name-directory (buffer-file-name)) t))
              \n
              "\n")

;;;###autoload
(defun now-scala-fill-paragraph (&rest args)
  ;; move to inside multi-line comment or multi-line string, if outside
  (when (looking-at "\\s-*\\(?:/\\**\\|\"\"\"\\)\\s-*")
    (goto-char (match-end 0)))
  (let ((state (syntax-ppss))
        (fill-paragraph-function
         ;; Avoid infinite recursion, set fill-paragraph-function to
         ;; nil if it is 'scala-paragraph:fill-paragraph
         (unless (eq fill-paragraph-function 'scala-paragraph:fill-paragraph)
           fill-paragraph-function)))
    (cond ((integerp (nth 4 state))
           ;; mask multi-line comments and fill
           (save-restriction
             (narrow-to-region (save-excursion (goto-char (nth 8 state))
                                               (if (looking-back "^\\s-+")
                                                   (point-at-bol)
                                                 (point)))
                               (save-excursion (goto-char (nth 8 state))
                                               (if (forward-comment 1)
                                                   (point)
                                                 (point-max))))
             (apply #'fill-paragraph args))
           t)
          ((eq (nth 4 state) t)
           ;; line comment, let normal fill-function handle this
           nil)
          ((eq (nth 3 state) t)
           ;; mask multi-line strings and fill.
           (save-restriction
             (narrow-to-region (nth 8 state)
                               (save-excursion (goto-char (nth 8 state))
                                               (or (ignore-errors
                                                     (forward-sexp)
                                                     (point))
                                                   (point-max))))
             (apply #'fill-paragraph args))
           t)
          ;; TODO: fill lists
          ;; the rest should not be filled (code, etc)
          (t t))))
(provide 'now-scala-mode)

;;; now-scala-mode.el ends here
