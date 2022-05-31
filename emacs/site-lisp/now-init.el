(require 'whitespace)

;;;###autoload
(defun now-comment-auto-fill-only-comments ()
  "Set `comment-auto-fill-only-comments' to t."
  (setq comment-auto-fill-only-comments t))

;;;###autoload
(defun now-do-not-show-trailing-whitespace ()
  "Set `show-trailing-whitespace' to nil."
  (setq show-trailing-whitespace nil))

;;;###autoload
(defun now-set-fill-column-to-72 ()
  "Set `fill-column' to 72."
  (setq fill-column 72))

;;;###autoload
(defun now-set-fill-column-to-80 ()
  "Set `fill-column' to 80."
  (setq fill-column 80))

;;;###autoload
(defun now-set-smie-indent-basic-to-2 ()
  "Set `smie-indent-basic' to 2."
  (setq-local smie-indent-basic 2))

;;;###autoload
(defun now-tabulated-list-mode-use-global-glyphless-char-display ()
  "Kill the local variable `glyphless-char-display'."
  (kill-local-variable 'glyphless-char-display))

;;;###autoload
(defun now-remove-continuation-fringe-indicator ()
  "Remove `'continuation' from `fringe-indicator-alist'."
  (setf (alist-get 'continuation fringe-indicator-alist) nil))

;;;###autoload
(defun now-remove-truncation-fringe-indicator ()
  "Remove `'continuation' from `fringe-indicator-alist'."
  (setf (alist-get 'truncation fringe-indicator-alist) nil))

;;;###autoload
(defun now-set-tab-width-to-2 ()
  "Set `tab-width' to 2."
  (setq-local tab-width 2))

;;;###autoload
(defun now-remove-space-after-tab-from-whitespace-style ()
  (setq-local whitespace-style (cl-remove 'space-after-tab whitespace-style)))

;;;###autoload
(defun now-set-page-delimiter-to-three-semicolons ()
  "Set `page-delimiter' to ;;; .*\\n."
  (setq-local page-delimiter ";;; .*\n"))

;;;###autoload
(defun now-set-split-width-threshold-based-on-aspect-ratio (_)
  (setq split-width-threshold
        (pcase (cdr (assoc 'geometry (car (display-monitor-attributes-list))))
          ((and `(,_ ,_ ,width ,height) (guard (< width height))) nil)
          (_ 160))))

;;;###autoload
;;;###autoload
(defun now-disable-case-fold-search-around (next &rest args)
  "Set ‘case-fold-search’ to ‘nil’ and apply NEXT to ARGS."
  (let ((case-fold-search nil))
    (apply next args)))


;;;###autoload
(defun now-report-emacs-startup-time ()
  "Write a ‘message’ that reports the time it took to start Emacs."
  (interactive)
  (message "Emacs ready in %.2f seconds"
           (float-time (time-subtract after-init-time
                                      before-init-time))))

(provide 'now-init)
