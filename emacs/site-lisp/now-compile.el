(require 'compile)

;;;###autoload
(defun now-compilation-maven-highlight ()
  (let ((type (compilation-type '(4 . 5))))
    (compilation--note-type type)
    (symbol-value (aref [compilation-info-face
                         compilation-warning-face
                         compilation-error-face]
                        type))))

(provide 'now-compile)
