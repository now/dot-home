;;; now-iso-transl.el --- iso-transl customizations  -*- lexical-binding: t; -*-

;; Copyright © 2022  Nikolai Weibull

;; Author: Nikolai Weibull <now@disu.se>
;; Keywords: local

;;; Code:

;;;###autoload
(defun now-iso-transl-init ()
  "Initialize \\'iso-transl customizations."
  (iso-transl-define-keys
   (mapcar (lambda (p) (cons (car p) nil)) iso-transl-char-map))
  (setq iso-transl-dead-key-alist nil)
  (defun iso-transl-define-keys (alist)
    (while alist
      (let ((translated-vec (cdr (car alist))))
        (define-key iso-transl-ctl-x-8-map (car (car alist)) translated-vec))
      (setq alist (cdr alist))))
  (iso-transl-define-keys iso-transl-char-map)
  (setf (alist-get "now" iso-transl-language-alist nil nil #'equal)
        '((".")
          (".." . [?·])
          (".3" . [?…])
          ("*'" . [?ʹ])
          ("*\"" . [?ʺ])))
  (iso-transl-set-language "now"))

(provide 'now-iso-transl)
;;; now-iso-transl.el ends here
