(add-to-list 'load-path (concat user-emacs-directory "site-lisp"))
(add-to-list 'load-path (concat user-emacs-directory "site-lisp/org"))
(require 'userloaddefs)
;; The order is important; package has to come last.
(dolist (feature '(provided-delayed-inits
                   unprovided-delayed-inits
                   package
                   disp-table))
  (load (concat user-emacs-directory "inits/" (symbol-name feature))))

;;; Interface

(setq-default indicate-buffer-boundaries '((bottom . left))
              mode-line-buffer-identification (propertized-buffer-identification "%b")
              mode-line-modes (butlast mode-line-modes) ; NOTE not buffer-local
              mode-line-format '(""
                                 mode-line-buffer-identification
                                 (:propertize " " 'help-echo help-echo)
                                 mode-line-modes))

(setq eol-mnemonic-unix ""
      eol-mnemonic-mac "␍"
      eol-mnemonic-dos "␍␊"
      eol-mnemonic-undecided "?")

(hide-mode-line)

(eval-after-load 'menu-bar
  '(menu-bar-mode -1))

(eval-after-load 'tool-bar
  '(tool-bar-mode -1))

(setq overlay-arrow-string "⇒")

(show-paren-mode 1)

(setq-default show-trailing-whitespace t)
(defun now-do-not-show-trailing-whitespace ()
  (setq show-trailing-whitespace nil))

(set-terminal-parameter nil 'background-mode 'light)
(load-theme 'now t)

(fset 'yes-or-no-p 'y-or-n-p)

(add-to-list 'display-buffer-alist
             '("\`\*magit-\(diff\|rev\): " . (display-buffer-pop-up-window)))

;;; Completion

(setq completion-show-help nil
      completions-format 'vertical)

(ido-mode 1)

(ido-ubiquitous-mode 1)

(flx-ido-mode 1)
(setq gc-cons-threshold 20000000)

(smex-initialize)

;;; Functionality

(setq-default fill-column 79)
(auto-fill-mode 1)

(setq vc-handled-backends nil)
(defun vc-git-mode-line-string (file) "")

(setq history-length 512)

(setq-default indent-tabs-mode nil)

(desktop-save-mode 1)

(evil-mode 1)

(global-set-key (kbd "C-x C-o") 'other-window)

(define-key global-map "\C-s" 'isearch-forward-regexp)
(define-key esc-map "\C-s" 'isearch-forward)
(define-key global-map "\C-r" 'isearch-backward-regexp)
(define-key esc-map "\C-r" 'isearch-backward)

(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode)
(add-hook 'ielm-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'slime-repl-mode-hook 'enable-paredit-mode)

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
