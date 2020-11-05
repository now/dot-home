;; now-scala-mode.el --- Scala-mode configuration  -*- lexical-binding: t -*-

(require 'skeleton)

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
