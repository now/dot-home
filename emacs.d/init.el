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

(global-set-key (kbd "C-x C-o") 'ace-window)
(global-set-key [f1] nil)
(global-set-key [f2] nil)
(global-set-key [f3] nil)
(global-set-key [f4] nil)

(add-to-list 'auto-mode-alist (cons (purecopy "\\.at\\'") 'autotest-mode))
(add-to-list 'auto-mode-alist (cons (purecopy "\\.jsx\(inc\)?\\'") 'js-mode))
(add-to-list 'auto-mode-alist (cons (purecopy "\\.rng\\'") 'xml-mode))
(add-to-list 'auto-mode-alist (cons (purecopy "\\.sch\\'") 'xml-mode))
(add-to-list 'auto-mode-alist (cons (purecopy "\\.xsd\\'") 'xml-mode))

(add-hook 'desktop-after-read-hook 'desktop-auto-save-enable)

(require 'mu4e)
