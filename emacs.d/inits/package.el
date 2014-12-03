(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/")
             'append)
(setq package-load-list '((ace-jump-mode t)
                          (auto-complete t)
                          (evil t)
                          (flx t)
                          (flx-ido t)
                          (goto-last-change t)
                          (ido-ubiquitous t)
                          (iedit t)
                          (magit t)
                          (multiple-cursors t)
                          (org t)
                          (paredit t)
                          (popup t)
                          (smex t)
                          (undo-tree t)))
(cond
 ((eq system-type 'cygwin)
  (add-to-list 'package-load-list '(cygwin-mount t))))
(package-initialize t)
(dolist (p package-load-list)
  (unless (package-installed-p (car p))
    (condition-case nil
        (package-install (car p))
      (error (package-refresh-contents)
             (package-install (car p))))))
(package-initialize)
