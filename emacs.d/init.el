(add-to-list 'load-path (concat user-emacs-directory "site-lisp"))
(add-to-list 'load-path (concat user-emacs-directory "site-lisp/org"))
(require 'userloaddefs)
(dolist (feature '(package
                   provided-delayed-inits
                   unprovided-delayed-inits
                   disp-table))
  (load (concat user-emacs-directory "inits/" (symbol-name feature))))

(setq mode-line-client `(""
                         (:propertize ("" (:eval (if server-buffer-clients "@" "")))
                                      help-echo ,(purecopy "emacsclient frame")))
      mode-line-front-space '(:eval (if (display-graphic-p)
                                        (concat (propertize "\u200b" 'display
                                                            '((raise -0.125)
                                                              (height 1.25)))
                                                " ")
                                      "-")))
(setq-default indent-tabs-mode nil
              indicate-buffer-boundaries t)
(setq gc-cons-threshold 20000000
      overlay-arrow-string "►"
      switch-to-buffer-preserve-window-point t)
(column-number-mode)
(desktop-save-mode)
(hide-mode-line)
(evil-mode)
(ido-mode)
(ido-ubiquitous-mode)
(flx-ido-mode)
(global-company-mode)
(load-theme 'now t)
(show-paren-mode)

(global-set-key (kbd "C-x C-o") 'other-window)

(add-to-list 'auto-mode-alist (cons (purecopy "\\.sch\\'") 'xml-mode))
(add-to-list 'auto-mode-alist (cons (purecopy "\\.xsd\\'") 'xml-mode))
(add-to-list 'auto-mode-alist (cons (purecopy "\\.at\\'") 'm4-mode))
(add-to-list 'auto-mode-alist (cons (purecopy "\\.jsx\(inc\)?\\'") 'js-mode))

(add-to-list 'xref-backend-functions 'gxref-xref-backend)

(defun smie-auto-fill (do-auto-fill)
  (if comment-auto-fill-only-comments
      (funcall do-auto-fill)
    (let ((fc (current-fill-column)))
      (when (and fc (> (current-column) fc))
        ;; The loop below presumes BOL is outside of strings or comments.  Also,
        ;; sometimes we prefer to fill the comment than the code around it.
        (unless (or (nth 8 (save-excursion
                             (syntax-ppss (line-beginning-position))))
                    (nth 4 (save-excursion
                             (move-to-column fc)
                             (syntax-ppss))))
          (while
              (and (with-demoted-errors
                       (save-excursion
                         (let ((end (point))
                               (bsf nil)    ;Best-so-far.
                               (gain 0))
                           (beginning-of-line)
                           (while (progn
                                    (smie-indent-forward-token)
                                    (and (<= (point) end)
                                         (<= (current-column) fc)))
                             ;; FIXME?  `smie-indent-calculate' can (and often
                             ;; does) return a result that actually depends on the
                             ;; presence/absence of a newline, so the gain computed
                             ;; here may not be accurate, but in practice it seems
                             ;; to work well enough.
                             (skip-chars-forward " \t")
                             (let* ((newcol (smie-indent-calculate))
                                    (newgain (- (current-column) newcol)))
                               (when (> newgain gain)
                                 (setq gain newgain)
                                 (setq bsf (point)))))
                           (when (> gain 0)
                             (goto-char bsf)
                             (newline-and-indent)
                             'done))))
                   (> (current-column) fc))))
        (when (> (current-column) fc)
          (funcall do-auto-fill))))))
