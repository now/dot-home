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
(hide-mode-line)
(load-theme 'now t)

(global-set-key (kbd "C-x C-o") 'other-window)

(add-to-list 'auto-mode-alist (cons (purecopy "\\.xsd\\'") 'xml-mode))
(add-to-list 'auto-mode-alist (cons (purecopy "\\.at\\'") 'm4-mode))
(add-to-list 'auto-mode-alist (cons (purecopy "\\.jsx\(inc\)?\\'") 'js-mode))

(defun rename-shows ()
  (interactive)
  (save-excursion
    (save-restriction
      (goto-char 1)
      (re-search-forward "^[^/]")
      (let ((n (- (count-lines 1 (point)) 1))
            (m (count-lines (point) (point-max))))
        (if (not (= n m))
            (error "Number of files doesn’t match number of names: %d≠%d" n m))
        (dotimes (i n)
          (goto-char 1)
          (beginning-of-line (+ n 1))
          (let ((line (downcase (replace-regexp-in-string ".*\"\\([^\"]+\\)\".*" "\\1"
                                                          (delete-and-extract-region (point) (line-end-position))
                                                          t))))
            (unless (eobp)
              (delete-char 1))
            (goto-char 1)
            (beginning-of-line (+ i 1))
            (re-search-forward "^/\\(.*?E\\([0-9]+\\).*?\\)\\.[^./]*/")
            (replace-match (concat "\\2-"
                                     (save-match-data
                                       (dolist (r '(("'" . "’") (" " . "_") ("\\.\\.\\." . "…") ("?" . "") ("&" . "and") ("!" . "") ("$" . "s") ("\\\\" "\\\\")) line)
                                         (setq line (replace-regexp-in-string (car r) (cdr r) line t t)))))
                             t nil nil 1)))))))
