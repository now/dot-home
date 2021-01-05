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

(provide 'now-scala-mode)

;;; now-scala-mode.el ends here
