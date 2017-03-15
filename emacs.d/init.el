(add-to-list 'load-path (concat user-emacs-directory "site-lisp"))
(add-to-list 'load-path (concat user-emacs-directory "site-lisp/org"))
(require 'userloaddefs)
(dolist (feature '(package
                   provided-delayed-inits
                   unprovided-delayed-inits
                   disp-table))
  (load (concat user-emacs-directory "inits/" (symbol-name feature))))

(setq mode-line-front-space
      '(:eval (if (display-graphic-p)
                  (concat (propertize "\u200b" 'display '((raise -0.125)
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
(loccur-mode)
(show-paren-mode)

(global-set-key (kbd "C-x C-o") 'other-window)

(add-to-list 'auto-mode-alist (cons (purecopy "\\.sch\\'") 'xml-mode))
(add-to-list 'auto-mode-alist (cons (purecopy "\\.xsd\\'") 'xml-mode))
(add-to-list 'auto-mode-alist (cons (purecopy "\\.at\\'") 'm4-mode))
(add-to-list 'auto-mode-alist (cons (purecopy "\\.jsx\(inc\)?\\'") 'js-mode))

(add-to-list 'xref-backend-functions 'gxref-xref-backend)
