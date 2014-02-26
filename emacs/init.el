(eval-when-compile
  (require 'cl))

(defun close-buffer-and-window-unless-last ()
  (interactive)
  (let* ((buffer (current-buffer))
         (window (get-buffer-window buffer))
         (next (next-window window)))
    (kill-buffer buffer)
    (when (and window
               (not (eq window next)))
      (delete-window window))))

(add-to-list 'load-path (concat user-emacs-directory "site-lisp"))
(require 'userloaddefs)

(dolist (feature '(bs
                   calc
                   cc-mode
                   css-mode
                   desktop
                   diff
                   diff-mode
                   dired
                   dired-aux
                   evil
                   grep
                   ido
                   ispell
                   magit
                   make-mode
                   man
                   nxml-mode
                   org
                   paredit
                   recentf
                   ruby-mode
                   sh-script))
  (eval-after-load feature `(load (concat user-emacs-directory "delayed-inits/" ,(symbol-name feature)))))

(dolist (feature '(package))
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
      eol-mnemonic-dos "␍␤"
      eol-mnemonic-undecided "?")

(hide-mode-line)
(add-hook 'window-setup-hook 'hide-mode-line-update)

(eval-after-load 'scroll-bar
  '(scroll-bar-mode -1))

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

(eval-after-load 'hideshow
  '(setq hs-set-up-overlay (lambda (ov)
                             (overlay-put ov 'display " …"))))
(setq overlay-arrow-string "⇒")

(eval-after-load 'paren
  '(setq show-paren-delay 0))
(show-paren-mode 1)

(setq-default show-trailing-whitespace t)
(defun now-do-not-show-trailing-whitespace ()
  (setq show-trailing-whitespace nil))
(dolist (hook '(Info-mode-hook
                compilation-mode-hook
                diff-mode-hook
                magit-mode-hook))
  (add-hook hook 'now-do-not-show-trailing-whitespace))

(set-terminal-parameter nil 'background-mode 'light)
(load-theme 'now t)

(fset 'yes-or-no-p 'y-or-n-p)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(setq pop-up-windows nil)

(defvar xterm-standard-colors)
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

(icomplete-mode 1)
(defun now-completion-delete-prompt ()
  (set-buffer standard-output)
  (goto-char (point-min))
  (delete-region (point) (search-forward "Possible completions are:\n")))
(add-hook 'completion-setup-hook 'now-completion-delete-prompt 'append)
(setq completion-show-help nil
      completions-format 'vertical)

(ido-mode 1)

(ido-ubiquitous-mode 1)

(defvar smex-save-file)
(setq smex-save-file (concat user-emacs-directory "smex-items"))
(smex-initialize)

;;; Functionality

(setq-default fill-column 79)
(auto-fill-mode 1)

(setq vc-handled-backends nil)

(setq history-length 512)

(setq make-backup-files nil
      require-final-newline 'visit-save)

(setq-default indent-tabs-mode nil)

(desktop-save-mode 1)

(defun call-interactively-at-git-root (command &optional record-flag keys)
  "Call COMMAND interactively with DEFAULT-DIRECTORY set to directory containing `.git'."
  (let ((root (locate-dominating-file (or (buffer-file-name) default-directory) ".git")))
    (if root
        (let ((default-directory root))
          (call-interactively command record-flag keys))
      (call-interactively command record-flag keys))))

(defun find-vc-project-file ()
  "Find a file, starting at the vc project root."
  (interactive)
  (call-interactively-at-git-root 'find-file))

(defun vc-project-shell-command ()
  "Run SHELL-COMMAND with DEFAULT-DIRECTORY set to VC-GIT-ROOT."
  (interactive)
  (call-interactively-at-git-root 'shell-command))

(evil-mode 1)

(global-set-key (kbd "C-x C-o") 'other-window)

(global-set-key (kbd "C-x i") 'iedit-mode)
(eval-after-load 'iedit
  '(define-key iedit-lib-keymap (kbd "M-o") 'iedit-toggle-unmatched-lines-visible))
(global-set-key (kbd "C-x I") 'mc/mark-next-like-this)

; (eval-after-load 'isearch …)
(define-key global-map "\C-s" 'isearch-forward-regexp)
(define-key esc-map "\C-s" 'isearch-forward)
(define-key global-map "\C-r" 'isearch-backward-regexp)
(define-key esc-map "\C-r" 'isearch-backward)

; (eval-after-load 'nxml …)
(add-to-list 'auto-mode-alist '("\\.xsd\\'" . nxml-mode))
(eval-after-load "rng-loc"
  '(add-to-list 'rng-schema-locating-files (concat user-emacs-directory "etc/schema/schemas.xml")))

(defvar org-enforce-todo-dependencies)
(setq org-enforce-todo-dependencies t)

(defvar recentf-save-file)
(setq recentf-save-file (concat user-emacs-directory "recentf"))
(recentf-mode t)

(eval-after-load 'compile
  '(setq compilation-auto-jump-to-first-error t
         compilation-scroll-output t
         compilation-ask-about-save nil))
(defun now-compilation-finish-autoclose (buffer string)
  (if (string-match "^finished" string)
      (delete-windows-on buffer)))
(add-to-list 'compilation-finish-functions 'now-compilation-finish-autoclose)
(defcustom compilation-mode-makefile-name "Makefile"
  "Name of Makefile to look for when using COMPILE-PACKAGE."
  :type 'string
  :group 'compilation)
(defun compile-package (&optional directory makefile-name)
  "Compile a package, looking from DIRECTORY upwards for a Makefile named MAKEFILE-NAME."
  (interactive)
  (let ((default-directory (compile-package-directory directory makefile-name)))
    (call-interactively 'compile)))
(defun compile-package-immediately (&optional command directory makefile-name)
  "Compile a package immediately, looking from DIRECTORY upwards for a Makefile named MAKEFILE-NAME."
  (interactive)
  (let ((default-directory (compile-package-directory directory makefile-name)))
    (compile (or command compile-command))))
(defun compile-package-directory (&optional directory makefile-name)
  "Find directory containing a Makefile named MAKEFILE-name, starting at DIRECTORY."
  (let ((directory (or directory default-directory))
        (makefile-name (or makefile-name compilation-mode-makefile-name))
        (root (expand-file-name "/")))
    (or (file-name-directory
         (expand-file-name makefile-name
                           (loop
                            for d = directory then (expand-file-name ".." d)
                            if (file-exists-p (expand-file-name makefile-name d))
                            return d
                            if (equal d root)
                            return nil)))
        directory)))

(add-to-list 'auto-mode-alist (cons (purecopy "\\.at\\'") 'm4-mode))

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of lisp code." t)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode)
(add-hook 'ielm-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'slime-repl-mode-hook 'enable-paredit-mode)

(autoload 'rnc-mode "rnc-mode")
(add-to-list 'auto-mode-alist (cons (purecopy "\\.rnc\\'") 'rnc-mode))

;; TODO This should be implemented by ruby-mode
(add-to-list 'auto-mode-alist (cons (purecopy "\\(?:\\`\\|/\\)Rakefile\\'") 'ruby-mode))

(eval-after-load 'compile
  '(progn
     (add-to-list 'compilation-error-regexp-alist-alist
                  '(autotest-header
                    "^\\([1-9][0-9]*\\. \\([^\n :]+\\.at\\):\\([1-9][0-9]*\\)\\): \\(?: FAILED\\|WARNIN\\(G\\)\\|\\(testing\\| ok\\)\\)"
                    2 3 nil (4 . 5) 1))
     (add-to-list 'compilation-error-regexp-alist 'autotest-header)
     (add-to-list 'compilation-error-regexp-alist-alist
                  '(autotest-check
                    "^\\(\\([^\n :]+\\.at\\):\\([1-9][0-9]*\\)\\): "
                    2 3 nil 0 1))
     (add-to-list 'compilation-error-regexp-alist 'autotest-check)
     (add-to-list 'compilation-error-regexp-alist-alist
                  '(autotest-check-error
                    "^\\(\\([^\n :]+\\.at\\):\\([1-9][0-9]*\\)\\): [^\n]+\n--- "
                    2 3 nil 2 1))
     (add-to-list 'compilation-error-regexp-alist 'autotest-check-error)
     (add-to-list 'compilation-error-regexp-alist-alist
                  '(gnu
                    "^\\(?:[[:alpha:]][-[:alnum:].]+: ?\\|[ \t]+\\(?:in \\|from \\)\\)?\
\\([0-9]*[^0-9\n]\\(?:[^\n :]\\| [^-/\n]\\|:[^ \n]\\)*?\\): ?\
\\([0-9]+\\)\\(?:[.:]\\([0-9]+\\)\\)?\
\\(?:-\\([0-9]+\\)?\\(?:\\.\\([0-9]+\\)\\)?\\)?:\
\\(?: *\\(\\(?:Future\\|Runtime\\)?[Ww]arning\\|W:\\)\\|\
 *\\([Ii]nfo\\(?:\\>\\|rmationa?l?\\)\\|I:\\|instantiated from\\|[Nn]ote\\)\\|\
 *[Ee]rror\\|\[0-9]?\\(?:[^0-9\n]\\|$\\)\\|[0-9][0-9][0-9]\\)"
                    1 (2 . 4) (3 . 5) (6 . 7)))
     (add-to-list 'compilation-error-regexp-alist-alist
                  '(ruby-backtrace
                    "^[ \t]+\\(?:in \\|from \\)\
\\([0-9]*[^0-9\n]\\(?:[^\n :]\\| [^-/\n]\\|:[^ \n]\\)*?\\):\
\\([0-9]+\\)\\(?::in .*\\)?"
                    1 2 nil 0))
     (add-to-list 'compilation-error-regexp-alist 'ruby-backtrace)))

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
                                       (dolist (r '(("'" . "’") (" " . "_") ("\\.\\.\\." . "…") ("?" . "") ("&" . "and") ("\\\\" "\\\\")) line)
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
  (setq insert-directory-program "gls")))

(cond
 ((eq window-system nil)
  (eval-after-load 'menu-bar
    (menu-bar-mode -1)))
 ((eq window-system 'ns)
  (set-frame-font "DejaVu Sans Mono-14")
  (setq default-frame-alist
        '((font . "DejaVu Sans Mono-14") (left . 100) (width . 132) (height . 41)))
  (if (fboundp 'set-fontset-font)
      (set-fontset-font (frame-parameter nil 'font) 'symbol '("DejaVu Sans Mono" . "unicode-bmp"))))
 ((eq window-system 'w32)
  (set-frame-font "DejaVu Sans Mono-9")
  (setq default-frame-alist
        '((font . "DejaVu Sans Mono-9") (width . 98) (height . 70)))))