(add-to-list 'load-path (concat user-emacs-directory "site-lisp"))
(require 'userloaddefs)
;; The order is important; package has to come last.
(dolist (feature '(provided-delayed-inits
                   unprovided-delayed-inits
                   package))
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
(add-hook 'window-setup-hook 'hide-mode-line-update)

(eval-after-load 'menu-bar
    (menu-bar-mode -1))

(eval-after-load 'tool-bar
  '(tool-bar-mode -1))

(setq initial-scratch-message nil)

(require 'disp-table)
(defface wrap-glyph
  '((((min-colors 16) (class color))
     :foreground "blue")
    (t
     :inverse-video t))
  "Face for wrap glyph."
  :group 'basic-faces)
(set-display-table-slot standard-display-table 'wrap (make-glyph-code #x21A9 'wrap-glyph))
(set-display-table-slot standard-display-table 'selective-display (vector (make-glyph-code #x2026)))
(set-display-table-slot standard-display-table 'vertical-border (make-glyph-code #x2502))

(setq overlay-arrow-string "⇒")

(eval-when-compile
  (defvar show-paren-delay))
(setq show-paren-delay 0)
(show-paren-mode 1)

(setq-default show-trailing-whitespace t)
(defun now-do-not-show-trailing-whitespace ()
  (setq show-trailing-whitespace nil))
(dolist (hook '(Buffer-menu-mode-hook
                Info-mode-hook
                calendar-mode-hook
                compilation-mode-hook
                diff-mode-hook
                eww-mode-hook
                help-mode-hook
                magit-mode-hook
                package-menu-mode-hook
                term-mode-hook))
  (add-hook hook 'now-do-not-show-trailing-whitespace))

(set-terminal-parameter nil 'background-mode 'light)
(load-theme 'now t)

(fset 'yes-or-no-p 'y-or-n-p)

(setq pop-up-windows nil)

(eval-when-compile
  (defvar xterm-standard-colors))
(setq xterm-standard-colors
  '(("black"          0 (  0   0   0))
    ("red"            1 (149  22  22))
    ("green"          2 ( 37 115  37))
    ("yellow"         3 (118  96  32))
    ("blue"           4 ( 47  90 155))
    ("magenta"        5 ( 96  47 128))
    ("cyan"           6 ( 86 148 168))
    ("white"          7 (192 192 192))
    ("brightblack"    8 ( 24  24  24))
    ("brightred"      9 (240  38  38))
    ("brightgreen"   10 (  0 144   0))
    ("brightyellow"  11 (240 165   0))
    ("brightblue"    12 ( 32 128 192))
    ("brightmagenta" 13 (147  55  99))
    ("brightcyan"    14 (128 176 192))
    ("brightwhite"   15 (246 246 246))))

;;; Completion

(setq completion-show-help nil
      completions-format 'vertical)

(ido-mode 1)

(ido-ubiquitous-mode 1)

(flx-ido-mode 1)
(setq gc-cons-threshold 20000000)

(eval-when-compile
  (defvar smex-save-file))
(setq smex-save-file (concat user-emacs-directory "smex-items"))
(smex-initialize)

;;; Functionality

(setq-default fill-column 79)
(auto-fill-mode 1)

(setq vc-handled-backends nil)
(defun vc-git-mode-line-string (file) "")

(setq history-length 512)

(setq make-backup-files nil
      require-final-newline 'visit-save)

(setq-default indent-tabs-mode nil)

(desktop-save-mode 1)

(evil-mode 1)

(global-set-key (kbd "C-x C-o") 'other-window)

(define-key global-map "\C-s" 'isearch-forward-regexp)
(define-key esc-map "\C-s" 'isearch-forward)
(define-key global-map "\C-r" 'isearch-backward-regexp)
(define-key esc-map "\C-r" 'isearch-backward)

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of lisp code." t)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode)
(add-hook 'ielm-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'slime-repl-mode-hook 'enable-paredit-mode)

(add-to-list 'auto-mode-alist (cons (purecopy "\\.xsd\\'") 'nxml-mode))
(add-to-list 'auto-mode-alist (cons (purecopy "\\.at\\'") 'm4-mode))
(add-to-list 'auto-mode-alist (cons (purecopy "\\(?:\\`\\|/\\)Rakefile\\'") 'ruby-mode))

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

(defun nordea-csv-row-to-org-table-row (&optional start end)
  (interactive "r")
  (if start
      (progn
        (setq start (min start end))
        (goto-char start)
        (setq end (copy-marker (max start end))))
    (if (and (called-interactively-p 'any) transient-mark-mode mark-active)
        (setq start (region-beginning)
              end (copy-marker (region-end)))
      (setq start (point)
            end (point-max-marker)))
    (goto-char start))
  (save-excursion
    (while (and (< (point) end)
                (re-search-forward "^\\(?:[^,]*,\\)\\{3\\}\"\\([^\"]+\\)\"\\(?:,\"[^\"]+\"\\)?$" end t))
      (replace-match "\\1"))
    (goto-char start)
    (while (and (< (point) end)
                (re-search-forward "\\." end t))
      (replace-match ""))
    (goto-char start)
    (while (and (< (point) end)
                (re-search-forward "," end t))
      (replace-match "."))
    (goto-char start)
    (while (and (< (point) end)
                (re-search-forward "^." end t))
      (if (string= (match-string 0) "-")
          (replace-match " " t t)
        (replace-match "-\\&"))))
  (set-marker end nil)
  nil)

(cond
 ((eq system-type 'cygwin)
  (windows-path-activate))
 ((eq system-type 'darwin)
  (setq insert-directory-program "a")))
