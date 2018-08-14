(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/")
             'append)
;; TODO Replace with (use-package … :ensure t)?
(setq package-load-list '((ace-window t)
                          (amx t)
                          (async t)
                          (avy t)
                          (bind-key t)
                          (company t)
                          (counsel t)
                          (dash t)
                          (diminish t)
                          (evil t)
                          (flx t)
                          (ghub t)
                          (git-commit t)
                          (goto-chg t)
                          (gxref t)
                          (ivy t)
                          (let-alist t)
                          (magit t)
                          (magit-popup t)
                          (memoize t)
                          (org t)
                          (paredit t)
                          (s t)
                          (smtpmail-multi t)
                          (swiper t)
                          (undo-tree t)
                          (use-package t)
                          (visual-fill-column t)
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
