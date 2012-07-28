; TODO: Remove once we switch to Emacs 24
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

(labels ((build-path (&rest components)
                     (let ((directories (mapcar #'file-name-as-directory (butlast components)))
                           (file (car (last components))))
                       (concat (apply #'concat directories) file))))
  ; TODO Change lisp to site-lisp
  (let ((my-share-emacs-path (build-path (expand-file-name "~") "share" "emacs" "lisp")))
    (setq-default custom-theme-directory (build-path my-share-emacs-path "themes"))
    (add-to-list 'load-path my-share-emacs-path)
    (require 'userloaddefs)
    (dolist (path '("evil"
                    "evil/lib"
                    "magit"
                    "ned"
                    "nxhtml/util"
                    "progmodes"))
      (add-to-list 'load-path (build-path my-share-emacs-path path)))
    (labels ((load-rc (missing-ok &rest components)
                      (load (apply #'build-path my-share-emacs-path "rc" components) missing-ok)))
      (load-rc t "os" (symbol-name system-type))
      (load-rc t "ws" (symbol-name window-system)))))

;;; Interface

(setq-default indicate-buffer-boundaries '((bottom . left))
              mode-line-buffer-identification (propertized-buffer-identification "%b")
              mode-line-modes (butlast mode-line-modes) ; NOTE not buffer-local
              mode-line-format '(""
                                 mode-line-client
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

(blink-cursor-mode -1)

(eval-after-load 'paren
  '(setq show-paren-delay 0))
(show-paren-mode 1)

(setq-default show-trailing-whitespace t)
(defun now-do-not-show-trailing-whitespace ()
  (setq show-trailing-whitespace nil))
(dolist (hook '(compilation-mode-hook
                diff-mode-hook
                magit-mode-hook))
  (add-hook hook 'now-do-not-show-trailing-whitespace))

(load-theme 'now t)

(fset 'yes-or-no-p 'y-or-n-p)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(setq pop-up-windows nil)

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
(ido-everywhere 1)
(eval-after-load 'ido
  '(setq ido-enable-flex-matching t
         ido-enable-last-directory-history nil
         ido-auto-merge-work-directories-length -1
         ido-use-filename-at-point nil))

(smex-initialize)

(setq history-length 512)

; (eval-after-load 'startup …)
(setq auto-save-list-file-prefix "~/.cache/emacs/auto-save-list/.saves-")

(eval-after-load 'desktop
  '(progn
     (setq desktop-path '("~/.cache/emacs")
           desktop-dirname (car desktop-path))
     (dolist (variable '(command-history
                         compile-history
                         evil-ex-history
                         evil-ex-search-history
                         read-expression-history
                         shell-command-history))
       (add-to-list 'desktop-globals-to-save variable))))
(desktop-save-mode 1)

(eval-after-load 'dired
  '(setq dired-isearch-filenames 'dwim
         dired-dwim-target t))

; evil
(eval-after-load 'evil-digraphs
  '(setq evil-digraphs-table-user
         '(((?c ?b) . ?\x2022)
           ((?t ?b) . ?\x2023)
           ((?\( ?/) . ?\x2209)
           ((?. ?3) . ?\x2026)
           ((?, ?3) . ?\x22ef)
           ((?< ?Y) . ?\x227a)
           ((?< ?/) . ?\x27e8)
           ((?> ?/) . ?\x27e9))))

(setq-default evil-shift-width 2)

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

(eval-after-load 'evil-vars
  '(delete 'shell-mode evil-insert-state-modes))

(eval-after-load 'evil-maps
  '(progn
     (define-key evil-normal-state-map "q" 'delete-other-windows)
     (define-key evil-normal-state-map "Q" 'evil-record-macro)

     (define-key evil-normal-state-map "s" nil)
     (define-key evil-motion-state-map "l" nil)
     (define-key evil-normal-state-map "l" 'evil-substitute)
     (define-key evil-motion-state-map "s" 'evil-forward-char)

     (define-key evil-normal-state-map "S" nil)
     (define-key evil-motion-state-map "L" nil)
     (define-key evil-normal-state-map "L" 'evil-change-whole-line)
     (define-key evil-motion-state-map "S" 'evil-window-bottom)

     (define-key evil-normal-state-map "K" 'man)

     (define-key evil-motion-state-map " " 'evil-scroll-page-down)
     (define-key evil-normal-state-map (kbd "DEL") 'evil-scroll-page-up)

     (define-key evil-normal-state-map ",b" 'ido-switch-buffer)
     (define-key evil-normal-state-map ",B" 'bs-show)
     (define-key evil-normal-state-map ",d" 'dired)
     (define-key evil-normal-state-map ",k" 'ido-kill-buffer)
     (define-key evil-normal-state-map ",w" 'save-buffer)
     (define-key evil-normal-state-map ",W" 'save-some-buffers)
     (define-key evil-normal-state-map "U" 'undo-tree-redo)
     (define-key evil-normal-state-map ",u" 'undo-tree-visualize)
     (define-key evil-normal-state-map "\C-d" 'suspend-frame)
     (define-key evil-normal-state-map "g\C-g" 'ned-info-on-file)
     (define-key evil-normal-state-map ",e" 'find-file)
     (define-key evil-normal-state-map ",E" 'find-vc-project-file)

     (define-key evil-normal-state-map ",c" 'vc-project-shell-command)
     (define-key evil-normal-state-map ",C" 'shell-command)

     (define-key evil-normal-state-map ",o" 'org-capture)

     (define-key evil-normal-state-map ",m" 'compile-package-immediately)
     (define-key evil-normal-state-map ",n" 'next-error)
     (define-key evil-normal-state-map ",p" 'previous-error)
     (define-key evil-normal-state-map ",N" 'compilation-next-file)
     (define-key evil-normal-state-map ",P" 'compilation-previous-file)

     (define-key evil-insert-state-map "\C-d" 'evil-normal-state)
     (define-key evil-insert-state-map "\C-^" 'evil-buffer)

     (global-set-key (kbd "C-x C-o") 'other-window)

     (define-key evil-normal-state-map "`" 'smex)
     (define-key evil-motion-state-map "`" 'smex)

     (eval-after-load 'bs
       '(progn
          (evil-make-overriding-map bs-mode-map 'normal t)
          (evil-define-key 'motion bs-mode-map "h" 'evil-backward-char)
          (evil-define-key 'motion bs-mode-map "j" 'bs-down)
          (evil-define-key 'normal bs-mode-map "k" 'bs-up)
          (evil-define-key 'motion bs-mode-map "k" 'bs-up)
          (evil-define-key 'normal bs-mode-map "s" 'evil-forward-char)
          (evil-define-key 'motion bs-mode-map "s" 'evil-forward-char)
          (evil-define-key 'normal bs-mode-map "w" 'bs-save)))))


(defun evil-delete-auto-indent-on-insert-state-exit ()
  (if (and (eolp)
           (member last-command '(evil-ret
                                  evil-open-below
                                  evil-open-above
                                  reindent-then-newline-and-indent
                                  c-electric-semi&comma)))
      (delete-horizontal-space)))

(eval-after-load 'evil
  '(add-hook 'evil-insert-state-exit-hook 'evil-delete-auto-indent-on-insert-state-exit))

; TODO Move this before everything else and remove all then unnecessary eval-after-loads
(evil-mode 1)
;; TODO: This might not be needed anymore.
;;(define-key undo-tree-visualizer-map "s" 'undo-tree-visualize-switch-branch-right)
; end evil

; (eval-after-load 'files …)
(setq make-backup-files nil
      require-final-newline 'visit-save)

; (eval-after-load 'indent …)
(setq-default indent-tabs-mode nil)

; (eval-after-load 'info …)
(add-to-list 'Info-default-directory-list "~/share/info")

; (eval-after-load 'isearch …)
(define-key global-map "\C-s" 'isearch-forward-regexp)
(define-key esc-map "\C-s" 'isearch-forward)
(define-key global-map "\C-r" 'isearch-backward-regexp)
(define-key esc-map "\C-r" 'isearch-backward)

; (eval-after-load 'magit …)
; TODO Is this needed?
(autoload 'magit-status "magit" nil t)
(eval-after-load 'magit
  '(define-key magit-mode-map "q" 'close-buffer-and-window-unless-last))
(add-hook 'magit-log-edit-mode-hook
          (lambda ()
            (setq fill-column 72)))

(eval-after-load 'man
  '(setq Man-switches "-P cat"))

(autoload 'eruby-html-mumamo "mumamo-fun")
(eval-after-load 'mumamo
  '(setq mumamo-chunk-coloring 511))
(add-to-list 'auto-mode-alist '("html[/\\\\][^/\\\\]*\\.erb\\'" . eruby-html-mumamo))

; (eval-after-load 'nxml …)
(add-to-list 'auto-mode-alist '("\\.xsd\\'" . nxml-mode))
(eval-after-load "rng-loc"
  '(add-to-list 'rng-schema-locating-files "~/share/emacs/etc/schema/schemas.xml"))
(eval-after-load 'nxml-mode
  '(progn
     (setq nxml-slash-auto-complete-flag t)
     (define-abbrev-table 'nxml-mode-abbrev-table ()
       "Abbrev table in use in nXML mode buffers.")
     (define-abbrev nxml-mode-abbrev-table "xsls" "" 'nxml-mode-skeleton-xsl-stylesheet)
     (define-abbrev nxml-mode-abbrev-table "xslt" "" 'nxml-mode-skeleton-xsl-template)
     (add-hook 'nxml-mode-hook
               (lambda ()
                 (setq local-abbrev-table nxml-mode-abbrev-table)))
     (define-skeleton nxml-mode-skeleton-xsl-stylesheet
       "Insert an XSL Stylesheet."
       ""
       "<?xml version=\"1.0\" encoding=\"utf-8\"?>" \n
       > "<xsl:stylesheet version=\"1.0\" xmlns:xsl=\"http://www.w3.org/1999/XSL/Transform\">" \n
       > "<xsl:output method=\"xml\" encoding=\"utf-8\"/>" \n
       > _ \n
       "</xsl:stylesheet>" >)
     (define-skeleton nxml-mode-skeleton-xsl-template
       "Insert an XSL Template."
       ""
       > "<xsl:template match=\"" _ "\">" \n
       > "</xsl:template>" >)
     (defun now-nxml-complete-or-indent-for-tab-command ()
       "Try to perform nXML completion or, failing that, indent line or region."
       (interactive)
       (unless (run-hook-with-args-until-success 'nxml-completion-hook)
         (call-interactively 'indent-for-tab-command)))
     (define-key nxml-mode-map "\t" 'now-nxml-complete-or-indent-for-tab-command)
     (define-key nxml-mode-map "\C-j" 'reindent-then-newline-and-indent)))

(eval-after-load 'org
  '(progn
     (setq org-directory "~/Dropbox/Org"
           org-mobile-directory "~/Dropbox/MobileOrg"
           org-mobile-force-id-on-agenda-items nil
           org-refile-targets '((org-agenda-files . (:level . 1)))
           org-refile-allow-creating-parent-nodes 'confirm
           org-outline-path-complete-in-steps nil
           org-completion-use-ido t
           org-edit-src-persistent-message nil
           org-src-window-setup 'current-window
           org-src-fontify-natively t
           org-reverse-note-order t
           org-log-done 'time)
     (flet ((org-file (file)
                      (concat (file-name-as-directory org-directory) file)))
       (setq org-mobile-inbox-for-pull (org-file "from-mobile.org")
             org-default-notes-file (org-file "refile.org")
             org-agenda-files (list org-directory))
       (setq org-capture-templates
             '(("t" "Todo" entry (file "") "* TODO %?\n  %U\n  %i")
               ("T" "Annotated Todo" entry (file "") "* TODO %?\n  %U\n  %i\n  %a"))))))
(eval-after-load 'evil-maps
  '(progn
     (evil-declare-key 'motion org-mode-map
       (kbd "RET") 'org-cycle)
     (evil-declare-key 'normal org-mode-map
       ",<" 'org-mobile-pull
       ",>" 'org-mobile-push
       ",t" 'org-todo)))

(require 'recentf)
; (eval-after-load 'recentf …)
(global-set-key (kbd "C-x C-r") 'ido-find-recent-file)
(recentf-mode t)
(setq recentf-max-saved-items 50)
(defun ido-find-recent-file ()
  "Use `ido-completing-read' to \\[find-file] a recentf file"
  (interactive)
  (find-file (ido-completing-read "Find recent file: " recentf-list)))

; (eval-after-load 'simple …)
(setq-default fill-column 79)
(auto-fill-mode 1)

; (eval-after-load 'vc …)
(setq vc-handled-backends nil)

(eval-after-load 'diff
  '(progn
     (setq diff-switches "-u")))
(eval-after-load 'evil-core
  '(evil-declare-key 'normal diff-mode-map "q" 'close-buffer-and-window-unless-last))

; TODO: Add (add-to-list 'c-cleanup-list 'defun-close-semi)?
; We don’t need it right now.
(eval-after-load 'cc-mode
  '(progn
     (defconst now-c-style
       '("linux"
         (c-hanging-braces-alist . ((brace-list-open)
                                    (brace-list-close)
                                    (brace-entry-open)
                                    (substatement-open after)
                                    (block-close . c-snug-do-while)
                                    (arglist-cont-nonempty)
                                    (class-close)))
         (c-hanging-colons-alist . ((case-label after)
                                    (label after))))
       "now’s C Programming Style")
     (c-add-style "now-c-style" now-c-style)
     (setq c-default-style '((java-mode . "java")
                             (awk-mode . "awk")
                             (other . "now-c-style")))
     (define-key c-mode-base-map "\C-j" 'c-context-line-break)))

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

(eval-after-load 'grep
  '(progn
     (grep-apply-setting 'grep-command "grep -nH -P -e ")
     (evil-define-key 'normal grep-mode-map "q" 'close-buffer-and-window-unless-last)))

(eval-after-load 'make-mode
  '(setq makefile-backslash-align nil))

(autoload 'rnc-mode "rnc-mode")
(add-to-list 'auto-mode-alist (cons (purecopy "\\.rnc\\'") 'rnc-mode))

;; TODO This should be implemented by ruby-mode
(add-to-list 'auto-mode-alist (cons (purecopy "\\(?:\\`\\|/\\)Rakefile\\'") 'ruby-mode))
(eval-after-load 'ruby-mode
  '(progn
     (define-key ruby-mode-map "d" 'ruby-electric-end-character)
     (define-key ruby-mode-map "e" 'ruby-electric-end-character)
     (define-key ruby-mode-map "f" 'ruby-electric-end-character)))
(eval-after-load 'evil-core
  '(progn
     (evil-declare-key 'normal ruby-mode-map ",t" 'ruby-find-other-file)
     (evil-declare-key 'normal ruby-mode-map ",M" 'ruby-run-test-at-line)))
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
;(require 'flymake)
;(defun flymake-ruby-init ()
;  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
;                       'flymake-create-temp-inplace))
;         (local-file  (file-relative-name
;                       temp-file
;                       (file-name-directory buffer-file-name))))
;    (list "ruby" (list "-wc" local-file))))
;
;(push '(".+\\.rb$" flymake-ruby-init) flymake-allowed-file-name-masks)
;(push '("Rakefile$" flymake-ruby-init) flymake-allowed-file-name-masks)
;
;;; TODO: I mean, shouldn't this be local to each flymake-file-name-mask?  This
;;; is rather stupid.
;(push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3) flymake-err-line-patterns)
;
;(add-hook 'ruby-mode-hook
;          (lambda ()
;            ;; Don't want flymake mode for ruby regions in rhtml files and also on read only files
;            (if (and (not (null buffer-file-name)) (file-writable-p buffer-file-name))
;                (flymake-mode))
;            (define-key ruby-mode-map "\C-m" 'ruby-reindent-then-newline-and-indent)
;            ))

(eval-after-load 'sh-script
  '(progn
     (setq sh-indentation 2
           sh-basic-offset 2)
     (define-key sh-mode-map "\C-j" 'reindent-then-newline-and-indent)))

(eval-after-load 'css-mode
  '(setq css-indent-offset 2))
