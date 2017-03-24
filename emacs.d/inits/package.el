(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/")
             'append)
(setq package-load-list '((ace-jump-mode t)
                          (async t)
                          (company t)
                          (dash t)
                          (evil t)
                          (evil-multiedit t)
                          (flx t)
                          (flx-ido t)
                          (git-commit t)
                          (goto-chg t)
                          (gxref t)
                          (hyperbole t)
                          (ido-completing-read+ t)
                          (ido-ubiquitous t)
                          (iedit t)
                          (magit t)
                          (magit-popup t)
                          (org t)
                          (paredit t)
                          (s t)
                          (smex t)
                          (smtpmail-multi t)
                          (undo-tree t)
                          (with-editor t)))
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
