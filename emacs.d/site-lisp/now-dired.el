;; -*- lexical-binding: t -*-

(require 'dired-aux)
(require 'ediff)

;;;###autoload
(defun now-dired-ediff-files ()
  "Compare two marked files using `ediff-files'.
If only one file is marked, prompt for second file.  The newer of
the two files is passed as the first argument to `ediff-files'."
  (interactive)
  (let ((files (dired-get-marked-files))
        (windows (current-window-configuration)))
    (if (<= (length files) 2)
        (let* ((file1 (car files))
               (file2 (if (cdr files)
                          (cadr files)
                        (read-file-name
                         (format "Diff %s with: " file1)
                         (dired-dwim-target-directory)))))
          (if (file-newer-than-file-p file1 file2)
              (ediff-files file2 file1)
            (ediff-files file1 file2))
          (add-hook 'ediff-after-quit-hook-internal
                    (lambda ()
                      (setq ediff-after-quit-hook-internal nil)
                      (set-window-configuration windows))))
      (error "no more than 2 files should be marked"))))
