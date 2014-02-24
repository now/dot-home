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

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/")
             'append)
(setq package-load-list '((auto-complete t)
                          (evil t)
                          (goto-last-change t)
                          (ido-ubiquitous t)
                          (iedit t)
                          (magit t)
                          (multiple-cursors t)
                          (paredit t)
                          (popup t)
                          (smex t)
                          (undo-tree t)))
(package-initialize)

(dolist (feature '(bs
                   calc
                   cc-mode
                   desktop
                   diff
                   diff-mode
                   dired
                   dired-aux
                   grep
                   ido
                   ispell
                   man
                   nxml-mode))
  (eval-after-load feature `(load (concat user-emacs-directory "inits/" ,(symbol-name feature)))))

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

(declare-function ido-ubiquitous-mode "ido-ubiquitous.el")
(ido-ubiquitous-mode 1)

(defvar smex-save-file)
(setq smex-save-file (concat user-emacs-directory "smex-items"))
(declare-function smex-initialize "smex.el")
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

(defun evil-delete-auto-indent-on-insert-state-exit ()
  (if (and (eolp)
           (member last-command '(evil-ret
                                  evil-open-below
                                  evil-open-above
                                  reindent-then-newline-and-indent
                                  c-context-line-break
                                  c-electric-semi&comma)))
      (delete-horizontal-space)))

(eval-after-load 'evil
  '(progn
     (setq-default evil-shift-width 2)
     (setq evil-digraphs-table-user
           '(((?c ?b) . ?\x2022)
             ((?t ?b) . ?\x2023)
             ((?\( ?/) . ?\x2209)
             ((?. ?3) . ?\x2026)
             ((?, ?3) . ?\x22ef)
             ((?< ?Y) . ?\x227a)
             ((?< ?/) . ?\x27e8)
             ((?> ?/) . ?\x27e9)))

     (delete 'shell-mode evil-insert-state-modes)

     (add-hook 'evil-insert-state-exit-hook
               'evil-delete-auto-indent-on-insert-state-exit)

     (let ((define-keys (lambda (map key def &rest bindings)
                          (define-key map key def)
                          (if bindings
                              (apply define-keys map bindings)))))
       (funcall define-keys evil-normal-state-map
                "`" 'smex
                "~" 'smex-major-mode-commands
                "K" 'man
                "q" 'delete-other-windows
                "Q" 'evil-record-macro
                "U" 'undo-tree-redo

                "\C-d" 'suspend-frame

                ",a" 'org-agenda
                ",b" 'ido-switch-buffer
                ",B" 'bs-show
                ",c" 'vc-project-shell-command
                ",C" 'shell-command
                ",d" 'dired
                ",e" 'find-file
                ",E" 'find-vc-project-file
                ",k" 'ido-kill-buffer
                ",m" 'compile-package
                ",M" 'compile
                ",n" 'next-error
                ",N" 'compilation-next-file
                ",o" 'org-capture
                ",p" 'previous-error
                ",P" 'compilation-previous-file
                ",s" 'magit-status
                ",w" 'save-buffer
                ",W" 'save-some-buffers
                ",u" 'undo-tree-visualize

                "g\C-g" 'ned-info-on-file)
       (funcall define-keys evil-visual-state-map
                "\C-d" 'evil-normal-state)
       (funcall define-keys evil-insert-state-map
                "\C-d" 'evil-normal-state)
       (funcall define-keys evil-motion-state-map
                "`" 'smex
                "~" 'smex-major-mode-commands
                " " 'evil-scroll-page-down)
       ; TODO This should be done in evil-maps.el.  Oh, and it doesn’t work.
       (funcall define-keys evil-read-key-map
         "\C-k" 'evil-insert-digraph
         "\C-^" 'evil-buffer)
       ; TODO This doesn’t work either.
       (funcall define-keys evil-ex-search-keymap
         "\C-k" 'evil-insert-digraph)
       (funcall define-keys minibuffer-local-map
         "\C-k" 'evil-insert-digraph))

     (define-key evil-normal-state-map "s" nil)
     (define-key evil-motion-state-map "l" nil)
     (define-key evil-normal-state-map "l" 'evil-substitute)
     (define-key evil-motion-state-map "s" 'evil-forward-char)

     (define-key evil-normal-state-map "S" nil)
     (define-key evil-motion-state-map "L" nil)
     (define-key evil-normal-state-map "L" 'evil-change-whole-line)
     (define-key evil-motion-state-map "S" 'evil-window-bottom)

     (define-key evil-normal-state-map (kbd "DEL") 'evil-scroll-page-up)))
(declare-function evil-mode "evil-core.el")
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

(eval-after-load 'magit
  '(define-key magit-mode-map "q" 'close-buffer-and-window-unless-last))
(add-hook 'magit-log-edit-mode-hook
          (lambda ()
            (setq fill-column 72)))

; (eval-after-load 'nxml …)
(add-to-list 'auto-mode-alist '("\\.xsd\\'" . nxml-mode))
(eval-after-load "rng-loc"
  '(add-to-list 'rng-schema-locating-files (concat user-emacs-directory "etc/schema/schemas.xml")))

(defvar org-enforce-todo-dependencies)
(setq org-enforce-todo-dependencies t)

(eval-after-load 'org
  '(progn
     (defun now-refile-target-verify ()
       (not (member (nth 2 (org-heading-components)) org-done-keywords)))
     (setq ;org-agenda-span 'day
           org-archive-default-command 'org-archive-to-archive-sibling
           org-clock-out-remove-zero-time-clocks t
           org-directory "~/Documents/Google Drive/Org"
           org-link-frame-setup '((vm . vm-visit-folder)
                                  (gnus . org-gnus-no-new-news)
                                  (file . find-file)
                                  (wl . wl))
           org-loop-over-headlines-in-active-region 'region-start-level
           org-mobile-directory "~/Sites/dav"
           org-mobile-force-id-on-agenda-items nil
           org-refile-allow-creating-parent-nodes 'confirm
           org-refile-target-verify-function 'now-refile-target-verify
           org-refile-targets '((org-agenda-files . (:maxlevel . 9)))
           org-refile-use-outline-path 'file
           org-outline-path-complete-in-steps nil
           org-completion-use-ido t
           org-edit-src-persistent-message nil
           org-src-window-setup 'current-window
           org-src-fontify-natively t
           org-treat-S-cursor-todo-selection-as-state-change nil
           org-reverse-note-order t
           org-log-done 'time
           org-log-into-drawer t)
     ; TODO Move these to files so that MobileOrg and similar may see them
     (setq org-todo-keywords
           '((sequence "TODO(t)" "DONE(d)")
             (sequence "WAIT(w@/!)" "DLGT(g@/!)" "NIXD(c@/!)")
             (type "CALL")
             (type "CHAT")))
     (setq org-todo-keyword-faces
           '(("DLGT" . org-delegated)
             ("WAIT" . org-waiting)))
     (let ((org-file (lambda (file)
                       (concat (file-name-as-directory org-directory) file))))
       (setq org-mobile-inbox-for-pull (funcall org-file "from-mobile.org")
             org-default-notes-file (funcall org-file "refile.org")
             org-agenda-files (list org-directory)))
     (setq org-capture-templates
           '(("c" "Chat" entry (file "")
              "* CHAT with %? :CHAT:\n%U" :clock-in t :clock-resume t)
             ("p" "Phone Call" entry (file "")
              "* CALL %? :CALL:\n%U" :clock-in t :clock-resume t)
             ("t" "Todo" entry (file "")
              "* TODO %?\n  %U\n  %i" :clock-in t :clock-resume t)
             ("T" "Annotated Todo" entry (file "")
              "* TODO %?\n  %U\n  %i\n  %a" :clock-in t :clock-resume t)))
     (evil-define-key 'motion org-mode-map
       (kbd "RET") 'org-cycle)
     (evil-define-key 'normal org-mode-map
       ",<" 'org-mobile-pull
       ",>" 'org-mobile-push
       ",t" 'org-todo)))

(require 'recentf)
; (eval-after-load 'recentf …)
(global-set-key (kbd "C-x C-r") 'ido-find-recent-file)
(recentf-mode t)
(setq recentf-save-file (concat user-emacs-directory "recentf")
      recentf-max-saved-items 50)
(defun ido-find-recent-file ()
  "Use `ido-completing-read' to `find-file' a recentf file"
  (interactive)
  (find-file (ido-completing-read "Find recent file: " recentf-list)))

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

(eval-after-load 'make-mode
  '(setq makefile-backslash-align nil))

(add-to-list 'auto-mode-alist (cons (purecopy "\\.at\\'") 'm4-mode))

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of lisp code." t)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode)
(add-hook 'ielm-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'slime-repl-mode-hook 'enable-paredit-mode)

(eval-after-load 'paredit
  '(progn
     (evil-define-key 'normal paredit-mode-map
       "D" 'paredit-kill
       "gh" 'paredit-backward
       "gj" 'paredit-forward-up
       "gk" 'paredit-backward-up
       "gs" 'paredit-forward
       "x" 'paredit-forward-delete
       "X" 'paredit-backward-delete
       "))" 'paredit-forward-slurp-sexp
       ")}" 'paredit-forward-barf-sexp
       "((" 'paredit-backward-slurp-sexp
       "({" 'paredit-backward-barf-sexp
       "(J" 'paredit-join-sexps
       "(R" 'paredit-raise-sexp
       "(S" 'paredit-split-sexp
       "(s" 'paredit-splice-sexp
       "(W" 'paredit-wrap-sexp)))

(autoload 'rnc-mode "rnc-mode")
(add-to-list 'auto-mode-alist (cons (purecopy "\\.rnc\\'") 'rnc-mode))

;; TODO This should be implemented by ruby-mode
(add-to-list 'auto-mode-alist (cons (purecopy "\\(?:\\`\\|/\\)Rakefile\\'") 'ruby-mode))
(eval-after-load 'ruby-mode
  '(progn
     (dolist (key '("d" "e" "f"))
       (define-key ruby-mode-map key 'ruby-electric-end-character))
     (evil-define-key 'normal ruby-mode-map
       ",t" 'ruby-find-other-file
       ",M" 'ruby-run-test-at-line)))
(add-hook 'ruby-mode-hook
          (lambda ()
            (hs-minor-mode 1)
            (set (make-local-variable 'compile-command) "rake -s ")
            (set (make-local-variable 'compilation-mode-makefile-name) "Rakefile")
            (set (make-local-variable 'paragraph-start) "\f\\|[ \t]*$\\|[ \t]*#[ \t]*@[[:alpha:]]+[ \t]")
            (set (make-local-variable 'adaptive-fill-function)
                 (lambda ()
                   (if (looking-at "\\([ \t]*#[ \t]*\\)@[[:alpha:]]+[ \t]")
                       (concat (match-string 1) "  "))))))
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
(defcustom ruby-unit-test-file-name-mapping
  '(("\\(.*\\)\\(/lib/\\)\\(.*\\.\\(rb\\|treetop\\)\\)$" . "\\1/test/unit/\\3"))
  "Unit test file-name mapping."
  :type '(alist :value-type (group string))
  :group 'ruby)
(defcustom ruby-implementation-file-name-mapping
  '(("\\(.*\\)\\(/test/unit/\\)\\(.*\\)\\.rb$" . "\\1/lib/\\3.treetop")
    ("\\(.*\\)\\(/test/unit/\\)\\(.*\\)\\.rb$" . "\\1/lib/\\3.rb"))
  "Unit test file-name mapping."
  :type '(alist :value-type (group string))
  :group 'ruby)
(defun ruby-find-other-file (&optional file-name)
  (interactive)
  (let* ((file-name (or file-name (buffer-file-name)))
         (other-file-name (and file-name
                               (or (ruby-find-other-file-name file-name ruby-unit-test-file-name-mapping t)
                                   (ruby-find-other-file-name file-name ruby-implementation-file-name-mapping)))))
    (if (not other-file-name)
        (signal 'file-error (list "No other file for this buffer" file-name))
      (find-file other-file-name))))
(defun ruby-find-other-file-name (file-name mapping &optional missing-ok)
  (let ((replacement
         (loop for e in mapping
               if (and (string-match (car e) file-name)
                       (or missing-ok
                           (file-exists-p
                            (replace-match (cdr e) nil nil file-name nil))))
               return (cdr e))))
    (if replacement
        (replace-match replacement nil nil file-name nil))))
; TODO: Validate file-name
(defun ruby-run-test-at-line (&optional file-name line)
  "Run test at LINE."
  (interactive)
  (let* ((file-name (or file-name (buffer-file-name)))
         (test-file-name (if (ruby-find-other-file-name file-name ruby-unit-test-file-name-mapping t)
                             (ruby-find-other-file-name file-name ruby-unit-test-file-name-mapping)
                           file-name))
         (line (or line (count-lines (point-min) (point))))
         (line-as-string (if (ruby-find-other-file-name file-name ruby-implementation-file-name-mapping)
                             (number-to-string line))))
    (compile-package-immediately
     (concat
      "rake -s"
      " TEST=" test-file-name
      (if line-as-string (concat " LINE=" line-as-string) "")))))

(declare-function ruby-indent-line "ruby-mode.el")
(defun ruby-electric-end-character (arg)
  (interactive "P")
  (self-insert-command (prefix-numeric-value arg))
  (ruby-electric-possibly-adjust-indent))
(defun ruby-electric-possibly-adjust-indent ()
  (if (ruby-electric-adjustable-word-p)
    (save-excursion
      (ruby-indent-line t))))
(defun ruby-electric-adjustable-word-p ()
  (if (ruby-electric-code-at-point-p)
    (save-excursion
      (beginning-of-line)
      (looking-at "\\s-*\\(else\\|elsif\\|end\\|ensure\\|rescue\\)"))))
(defun ruby-electric-code-at-point-p ()
  (let ((properties (text-properties-at (point))))
    (and (null (memq 'font-lock-string-face properties))
         (null (memq 'font-lock-comment-face properties)))))

(defun ruby-file-name-to-module-name (&optional file-name)
  (let ((file-name (or file-name (buffer-file-name))))
    (mapconcat 'identity
               (mapcar 'capitalize
                       (split-string
                        (file-name-sans-extension
                         (or (and file-name
                                  (file-relative-name
                                   file-name
                                   (expand-file-name
                                    (concat
                                     (locate-dominating-file file-name ".git")
                                     "lib"))))
                             (buffer-name)))
                        "/"
                        t))
               "::")))
(eval-after-load 'ruby-mode
  '(progn
     (define-abbrev ruby-mode-abbrev-table "d" "" 'ruby-skeleton-def)
     (define-skeleton ruby-skeleton-def
       "Insert a method definition."
       "Method name and argument list: "
       > "def " str \n
       > _ \n
       "end" >)
     (define-abbrev ruby-mode-abbrev-table "tlc" "" 'ruby-skeleton-top-level-class)
     (define-skeleton ruby-skeleton-top-level-class
       "Insert a top-level class."
       ""
       "# -*- coding: utf-8 -*-" \n
       \n
       > "class " (ruby-file-name-to-module-name) \n
       > _ \n
       "end" >)
     (define-abbrev ruby-mode-abbrev-table "tlm" "" 'ruby-skeleton-top-level-module)
     (define-skeleton ruby-skeleton-top-level-module
       "Insert a top-level module."
       ""
       "# -*- coding: utf-8 -*-" \n
       \n
       "module " (ruby-file-name-to-module-name) \n
       > _ \n
       "end" >)
     (define-abbrev ruby-mode-abbrev-table "tle" "" 'ruby-skeleton-top-level-expectations)
     (define-skeleton ruby-skeleton-top-level-expectations
       "Insert top-level expectations."
       ""
       "# -*- coding: utf-8 -*-" \n
       \n
       "Expectations do" \n
       > "expect " _ " do" \n
       >  _ \n
       "end" > \n
       "end" >)))

(eval-after-load 'sh-script
  '(progn
     (setq sh-indentation 2
           sh-basic-offset 2)
     (define-key sh-mode-map "\C-j" 'reindent-then-newline-and-indent)))

(eval-after-load 'css-mode
  '(setq css-indent-offset 2))

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
