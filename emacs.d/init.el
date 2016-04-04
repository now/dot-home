(add-to-list 'load-path (concat user-emacs-directory "site-lisp"))
(add-to-list 'load-path (concat user-emacs-directory "site-lisp/org"))
(require 'userloaddefs)
(dolist (feature '(package
                   provided-delayed-inits
                   unprovided-delayed-inits
                   disp-table))
  (load (concat user-emacs-directory "inits/" (symbol-name feature))))

(setq-default indent-tabs-mode nil
              indicate-buffer-boundaries t)
(setq gc-cons-threshold 20000000
      overlay-arrow-string "►")
(show-paren-mode 1)
(ido-mode 1)
(ido-ubiquitous-mode 1)
(flx-ido-mode 1)
(desktop-save-mode 1)
(evil-mode 1)
(column-number-mode 1)
(hide-mode-line)
(load-theme 'now t)

(global-set-key (kbd "C-x C-o") 'other-window)

(add-to-list 'auto-mode-alist (cons (purecopy "\\.sch\\'") 'xml-mode))
(add-to-list 'auto-mode-alist (cons (purecopy "\\.xsd\\'") 'xml-mode))
(add-to-list 'auto-mode-alist (cons (purecopy "\\.at\\'") 'm4-mode))
(add-to-list 'auto-mode-alist (cons (purecopy "\\.jsx\(inc\)?\\'") 'js-mode))
