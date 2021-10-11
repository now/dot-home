;; -*- lexical-binding: t; -*-

(add-to-list 'load-path (concat user-emacs-directory "site-lisp"))
(require 'userloaddefs)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/")
             'append)

(setq ring-bell-function 'ignore)
(setq process-connection-type nil)
(setq gc-cons-threshold 20000000
      undo-limit 80000000
      undo-strong-limit 120000000
      undo-outer-limit 360000000)

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %.2f seconds"
                     (float-time (time-subtract after-init-time
                                     before-init-time)))))

;; TODO eval-when-compile?
(eval-and-compile
  (require 'use-package)
  (setq use-package-compute-statistics nil
        use-package-verbose nil))

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

;; This comes first, as we want path set early on.
(use-package now-path
  :when (eq (window-system) 'ns))

;; This comes first, as keybindings here, notably that of ‘,’, affect
;; other packages’ keybinding potentials.
(use-package evil
  :ensure t
  :custom ((evil-digraphs-table-user '(((?c ?b) . ?•)
                                       ((?t ?b) . ?‣)
                                       ((?\( ?/) . ?∉)
                                       ((?. ?3) . ?…)
                                       ((?, ?3) . ?⋯)
                                       ((?< ?Y) . ?≺)
                                       ((?< ?/) . ?⟨)
                                       ((?> ?/) . ?⟩)))
           (evil-echo-state nil)
           (evil-mode-line-fomat nil)
           (evil-move-cursor-back nil)
           (evil-shift-round nil)
           (evil-shift-width 2)
           (evil-symbol-word-search t)
           (evil-want-abbrev-expand-on-insert-exit nil)
           (evil-want-fine-undo t))
  :bind ((:map evil-insert-state-map
               ("C-d" . evil-normal-state))
         (:map evil-motion-state-map
               (",")
               ("<DEL>" . evil-scroll-page-up)
               ("<SPC>" . evil-scroll-page-down)
               (", B" . buffer-menu)
               ("C-d" . evil-insert)
               ("L")
               ("S" . evil-window-bottom)
               ("S-<SPC>" . evil-scroll-page-up)
               ("g b" . backward-sexp)
               ("g c" . evil-avy-goto-char-timer)
               ("g l" . evil-avy-goto-line)
               ("g s" . evil-avy-goto-symbol-1)
               ("g w" . forward-sexp)
               ("l")
               ("s" . evil-forward-char))
         (:map evil-normal-state-map
               ("<DEL>")
               ("L" . evil-change-whole-line)
               ("Q" . evil-record-macro)
               ("S")
               ("C-r")
               ("g w")
               ("l" . evil-substitute)
               ("s")
               ("q" . delete-other-windows)
               ("z C" . evil-hide-fold-rec))
         (:map evil-replace-state-map
               ("C-d" . evil-normal-state))
         (:map evil-visual-state-map
               ("C-d" . evil-normal-state)))
  :config (progn
            (setq evil-emacs-state-modes
                  (cl-set-difference evil-emacs-state-modes
                                     '(archive-mode
                                       git-commit-mode
                                       git-rebase-mode
                                       tar-mode
                                       xref--xref-buffer-mode))
                  evil-insert-state-modes
                  (cl-set-difference evil-insert-state-modes
                                     '(term-mode))
                  evil-overriding-maps
                  (cl-set-difference evil-overriding-maps
                                     '((compilation-mode-map . nil))
                                     :key 'car))
            (add-to-list 'evil-emacs-state-modes 'term-mode)
            (push '((docfold-minor-mode)
                    :open-all   docfold-show-all
                    :close-all  docfold-hide-all
                    :toggle     docfold-toggle-section
                    :open       docfold-show-section
                    :open-rec   docfold-show-section-recursively
                    :close      docfold-hide-section
                    :close-rec  docfold-hide-section-recursively)
                  evil-fold-list)))

(use-package evil-jumps
  :functions (evil--jumps-install-or-uninstall)
  :defer t
  :config (remove-hook 'evil-local-mode-hook
                       #'evil--jumps-install-or-uninstall))

(use-package now-evil
  ;; We include a function that’s defined by evil that’s later used in
  ;; :after evil use-packages.
  :functions (evil-change-state
              evil-fold-action
              evil-get-auxiliary-keymap ; TODO Figure out why this is required.
              evil-make-overriding-map ; TODO Figure out why this is required.
              evil-set-command-properties
              evil-set-initial-state)
  :hook ((evil-insert-state-exit . now-evil-delete-auto-indent))
  :config (evil-define-command evil-hide-fold-rec ()
              "Close fold at point recursively.
See also `evil-open-fold' and `evil-close-fold'."
              (evil-fold-action evil-fold-list :close-rec)))

(use-package avy
  :bind* ("C-." . avy-goto-char-timer)
  ;; TODO Probably use own setup code here instead.
  :config (avy-setup-default))

(use-package fira-code-mode
  :ensure t
  :custom ((fira-code-mode-disabled-ligatures '("[]" "x")))
  :hook ((prog-mode . fira-code-mode)))

(use-package iso-transl
  :defer t
  :config (progn
            (setf (alist-get "now" iso-transl-language-alist nil nil #'equal)
                  '((".")
                    (".." . [?·])
                    (".3" . [?…])
                    ("*'" . [?ʹ])
                    ("*\"" . [?ʺ])))
            (iso-transl-set-language "now")))

(use-package ivy
  :diminish
  :custom ((ivy-count-format "")
           (ivy-height 20)
           (ivy-use-virtual-buffers t))
  :bind (:map ivy-mode-map
              ("C-j" . ivy-immediate-done)
              ("C-M-j" . ivy-alt-done))
  :config (progn
            (setq ivy-re-builders-alist '((swiper . ivy--regex-plus)
                                          (t . ivy--regex-fuzzy)))
	    (declare-function ivy-mode "ivy")
            (ivy-mode 1)))

(use-package ivy-avy
  :ensure t)

(use-package now-ivy
  :after ivy
  :bind (:map ivy-mode-map
              ([remap switch-to-buffer] . 'now-ivy-switch-buffer)))

(use-package ace-window
  :ensure t
  :bind (("C-x C-o" . ace-window)
         ("C-x o" . ace-window)))

(use-package ace-window
  :after evil
  :bind ((:map evil-motion-state-map
                (", o" . ace-window))))

(use-package amx
  :ensure t)

(use-package amx
  :after evil
  :bind (:map evil-motion-state-map
              ("~" . amx-major-mode-commands)))

(use-package arc-mode
  :after evil
  :evil-bind ((:map motion archive-mode-map
                    ("j" . archive-next-line)
                    ("k" . archive-previous-line)
                    ("h" . evil-backward-char)))
  :config (progn
            (evil-make-overriding-map archive-mode-map 'normal)))

(use-package auth-source
  :when (eq system-type 'darwin)
  :no-require t
  :custom ((auth-sources '(macos-keychain-internet))))

(use-package auth-source
  :when (eq system-type 'gnu/linux)
  :no-require t
  :custom ((auth-sources '("secrets:Login"))))

(use-package autoinsert
  :config (progn
            (setq auto-insert-alist
                  (assoc-delete-all
                   nil auto-insert-alist
                   (lambda (key _)
                     (or (member (cdr-safe key)
                                 '("C / C++ header"
                                   "C / C++ program"
                                   "Makefile"))
                         (member key
                                 '(html-mode
                                   plain-tex-mode
                                   bibtex-mode
                                   latex-mode
                                   ada-mode))))))
            (auto-insert-mode)))

(use-package autorevert
  :diminish auto-revert-mode)

(use-package autotest-mode
  :mode ("\\.at\\'"))

(use-package bindings
  :no-require t
  :custom ((column-number-indicator-zero-based nil))
  :config (progn
            (setq mode-line-client `(""
                                     (:propertize
                                      (""
                                       (:eval
                                        (if (bound-and-true-p
                                             server-buffer-clients)
                                            "@"
                                          "")))
                                      help-echo
                                      "emacsclient frame")))))

(use-package buff-menu-ext
  :bind ((:map Buffer-menu-mode-map
               ("%")
               ("%f" . Buffer-menu-mark-by-file-name-regexp)
               ("%m" . Buffer-menu-mark-by-mode-regexp)
               ("%n" . Buffer-menu-mark-by-name-regexp)
               ("*/" . Buffer-menu-mark-dired-buffers)
               ("*M" . Buffer-menu-mark-by-mode)
               ("*c" . Buffer-menu-change-marks)
               ("*e" . Buffer-menu-mark-dissociated-buffers)
               ("*h" . Buffer-menu-mark-help-buffers)
               ("*m" . Buffer-menu-mark-modified-buffers)
               ("*r" . Buffer-menu-mark-read-only-buffers)
               ("*s" . Buffer-menu-mark-special-buffers)
               ("*u" . Buffer-menu-mark-unsaved-buffers)
               ("*z" . Buffer-menu-mark-compressed-file-buffers)
               ("." . Buffer-menu-mark-old-buffers)
               ("Q" . Buffer-menu-do-query-replace-regexp)
               ("U" . Buffer-menu-unmark-all)
               ("r" . Buffer-menu-toggle-read-only)))
  :evil-bind ((:map motion Buffer-menu-mode-map
                    ("s" . evil-forward-char)
                    ("w" . Buffer-menu-save))))

(use-package buffer
  :no-require t
  :custom ((indicate-buffer-boundaries t)))

(use-package bug-reference
  :custom ((bug-reference-bug-regexp "\\(\\b\\([A-Z]+-[0-9]+\\)\\b\\)")))

(use-package calc
  :no-require t
  :config (progn
            (require 'now-calc)
            (setq-default calc-group-char " "
                          calc-gnuplot-default-device "dumb"
                          calc-show-banner nil)))

(use-package calendar
  :custom ((calendar-date-style 'iso)
           (calendar-mark-holidays-flag t)
           (calendar-intermonth-text
            '(propertize
              (format "%2d"
                      (car (calendar-iso-from-absolute
                            (calendar-absolute-from-gregorian
                             (list month day year)))))
              'font-lock-face 'week))
           (calendar-week-start-day 1))
  :evil-bind ((:map motion calendar-mode-map
                    ("C-b" . calendar-scroll-right-three-months)
                    ("C-f" . calendar-scroll-left-three-months)
                    ("b" . calendar-beginning-of-week)
                    ("h" . calendar-backward-day)
                    ("j" . calendar-forward-week)
                    ("k" . calendar-backward-week)
                    ("s" . calendar-forward-day)
                    ("w" . calendar-end-of-week))))

(use-package cc-mode
  :custom ((c-default-style '((java-mode . "now-java-style")
                              (awk-mode . "awk")
                              (other . "now-c-style")))
           (c-electric-pound-behavior '(alignleft)))
  :bind ((:map c-mode-base-map
               ("C-j" . c-context-line-break)))
  :config (progn
            (c-add-style "now-c-style"
                         `("linux"
                           (c-hanging-colons-alist . ((label after)))
                           (c-offsets-alist . ((arglist-cont . +)
                                               (arglist-cont-nonempty . +)
                                               (cpp-define-intro . +)
                                               (inher-cont . +)
                                               (member-init-cont . +)
                                               (objc-method-args-cont . +)
                                               (objc-method-call-cont . +)
                                               (template-args-cont . +)))
                           (comment-start . "// ")
                           (comment-end . "")
                           (paragraph-start . ,(rx (| (: (* (in ?\  ?\t))
                                                         (| (>= 2 ?/)
                                                            (* ?*))
                                                         (* (in ?\ ?\t))
                                                         (| eol
                                                            (: (| (: ?@
                                                                     (+ (in alpha)))
                                                                  ?•)
                                                               (in ?\ ?\t))))
                                                      (: bol ?\f))))))
            (c-add-style "now-java-style"
                         `("java"
                           (c-offsets-alist . ((arglist-intro . ++)))
                           (whitespace-line-column . 100)))
            (defun now-c-mode-hook ()
              (setq-local adaptive-fill-function
                          'now-c-mode-adaptive-fill-function)
              (setq-local docfold-set-up-overlay
                          'docfold-c-set-up-overlay))))

(use-package now-cc-mode
  :bind ((:map c-mode-map
               ([remap c-fill-paragraph] . now-c-fill-paragraph)))
  :hook ((c-mode . now-c-mode-hook)
         (c-mode . now-c-auto-newline-mode)))

(use-package checkdoc
  :no-require t
  :custom ((checkdoc-arguments-in-order-flag t)
           (checkdoc-package-keywords-flag t)
           (checkdoc-spellcheck-documentation-flag t)))

(use-package company
  :ensure t
  :diminish
  :defer 1
  :custom ((company-format-margin-function nil)
           (company-idle-delay .175)
           (company-show-numbers t)
           (company-backends '(company-bbdb
                               company-semantic
                               company-cmake
                               company-capf
                               company-clang
                               company-files
                               (company-dabbrev-code
                                company-gtags
                                company-keywords)
                               company-dabbrev)))
  :config (progn
	    (declare-function global-company-mode "company")
            (global-company-mode)))

(use-package company-dabbrev
  :no-require t
  :custom ((company-dabbrev-downcase nil)
           (company-dabbrev-ignore-case nil)))

(use-package compile
  :custom ((compilation-context-lines 0)
           ;;(compilation-disable-input t)
           (compilation-error-regexp-alist '(sbt
                                             maven
                                             clang-include
                                             gcc-include
                                             gnu
                                             gcov-file
                                             gcov-header
                                             gcov-nomark
                                             gcov-called-line
                                             gcov-never-called))
           (compilation-scroll-output 'first-error))
  :config (progn
            (dolist (cons
                     `((maven
                        ,(rx bol
                             "["
                             (| "ERROR"
                                (group-n 4 (: "WARN" (? "ING")))
                                (group-n 5 "INFO"))
                             "]"
                             (+ ?\s)
                             (? "[" (or "Warn" "Error") "] ")
                             (group-n 1
                                      (* digit)
                                      (not (in digit ?\n))
                                      (*? (| (not (in ?\n ?\s ?:))
                                             (: ?\s (not (in ?- ?/ ?\n)))
                                             (: ?: (not (in ?\s ?\n))))))
                             ":"
                             (| (: "["
                                   (group-n 2 (+ digit))
                                   (? ","
                                      (group-n 3 (+ digit)))
                                   "]")
                                (: (group-n 2 (+ digit)) ":"
                                   (? (group-n 3 (+ digit)) ":")))
                             " ")
                        now-compilation-maven-file
                        2 3 (4 . 5) nil (1 (funcall 'now-compilation-maven-highlight)))
                       (sbt
                        ,(rx bol
                             "["
                             (| "error"
                                (group-n 4 (: "warn" (? "ing")))
                                (group-n 5 "info"))
                             "] "
                             (group-n 1
                                      (* digit)
                                      (not (in digit ?\n))
                                      (*? (| (not (in ?\n ?\s ?:))
                                             (: ?\s (not (in ?- ?/ ?\n)))
                                             (: ?: (not (in ?\s ?\n))))))
                             ":"
                             (| (: "["
                                   (group-n 2 (+ digit))
                                   ","
                                   (group-n 3 (+ digit))
                                   "]")
                                (: (group-n 2 (+ digit)) ":"
                                   (? (group-n 3 (+ digit)) ":")))
                             " ")
                        1 2 3 (4 . 5))
                       (typescript-X
                        ,(rx bol
                             (: (group-n 1
                                         (* (in (?0 . ?9)))
                                         (not (in (?0 . ?9) ?\n))
                                         (*? (| (not (in ?\n ?\s ?:))
                                                (: ?\s (not (in ?- ?/ ?\n)))
                                                (: ?: (not (in ?\s ?\n))))))
                                "("
                                (group-n 2 (+ (in (?0 . ?9))))
                                ","
                                (group-n 3 (+ (in (?0 . ?9))))
                                "): "))
                        1 2 3)
                       (typescript ;; webkit?
                        ,(rx bol
                             (: (group-n 1
                                         (* (in (?0 . ?9)))
                                         (not (in (?0 . ?9) ?\n))
                                         (*? (| (not (in ?\n ?\s ?:))
                                                (: ?\s (not (in ?- ?/ ?\n)))
                                                (: ?: (not (in ?\s ?\n))))))
                                "\n  Line "
                                (group-n 2 (+ (in (?0 . ?9))))
                                ":"
                                (group-n 3 (+ (in (?0 . ?9))))
                                ":  "))
                        1 2 3)
                       (typescript-following
                        ,(rx bol
                             (: "  Line "
                                (group-n 2 (+ (in (?0 . ?9))))
                                ":"
                                (group-n 3 (+ (in (?0 . ?9))))
                                ":  "))
                        nil 2 3)))
              (setf (alist-get (car cons) compilation-error-regexp-alist-alist)
                    (cdr cons)))
            (defun now-compilation-maven-file ()
	      (declare-function compilation--previous-directory "compile")
              (let ((s (match-string-no-properties 1)))
                (if (or (null s) (file-name-absolute-p s))
                    s
                  (let* ((pos (compilation--previous-directory
                               (match-beginning 0)))
                         (dir (when pos
                                (or (get-text-property (1- pos) 'compilation-directory)
                                    (get-text-property pos 'compilation-directory))))
                         (dir (when dir (file-name-as-directory (car dir)))))
                    (if dir
                        (if (file-exists-p (concat dir s))
                            s
                          (let* ((s (replace-regexp-in-string "\\$.*" "" s))
                                 (java-1 (format "%s.java" s))
                                 (scala-1 (format "%s.scala" s))
                                 (java-2 (format "%s.java"
                                                 (replace-regexp-in-string "\\." "/" s)))
                                 (scala-2 (format "%s.scala"
                                                  (replace-regexp-in-string "\\." "/" s))))
                            (cl-find-if
                             (lambda (f) (file-exists-p (concat dir f)))
                             (list
                              java-1
                              scala-1
                              java-2
                              scala-2
                              (format "src/main/java/%s" java-1)
                              (format "src/main/scala/%s" scala-1)
                              (format "src/test/java/%s" java-1)
                              (format "src/test/scala/%s" scala-1)
                              (format "src/main/java/%s" java-2)
                              (format "src/main/scala/%s" scala-2)
                              (format "src/test/java/%s" java-2)
                              (format "src/test/scala/%s" scala-2)))))
                      s)))))))

(use-package compile
  :after evil
  :bind ((:map evil-motion-state-map
               (", m" . compile)
               (", r" . recompile))
         (:map evil-motion-state-map
               (", N" . compilation-next-file)
               (", P" . compilation-previous-file))))

(use-package counsel
  :ensure t
  :functions (ivy-configure)
  ;; TODO After ivy?  I don’t quite see why that’d matter, but John
  ;; has it.
  :custom ((counsel-find-file-ignore-regexp
            (concat "\\(\\`\\.[^.]\\|"
                    (regexp-opt completion-ignored-extensions)
                    "\\'\\)")))
  :bind (("C-h f" . counsel-describe-function)
         ("C-h i" . counsel-info-lookup-symbol)
         ("C-h l" . counsel-find-library)
         ("C-h u" . counsel-unicode-char)
         ("C-h v" . counsel-describe-variable)
         ("C-x C-f" . counsel-find-file))
  :config (progn
            (ivy-configure 'counsel-evil-registers :height ivy-height)))

(use-package now-counsel-evil
  :after (counsel evil))

(use-package counsel
  :after evil
  :bind ((:map evil-motion-state-map
               (", D" . counsel-dired-jump)
               (", F" . counsel-git-grep)
               (", J" . counsel-git)
               (", \"" . counsel-evil-registers)
               (", j" . counsel-file-jump)
               ("M-d" . counsel-M-x)
               ("`" . counsel-M-x))
         (:map evil-visual-state-map
               ("`" . counsel-M-x))))

(use-package counsel
  :defines magit-mode-map
  :after magit
  :bind ((:map magit-mode-map
               (", B" . buffer-menu)
               (", D" . counsel-dired-jump)
               (", F" . counsel-git-grep)
               (", J" . counsel-git)
               (", b" . switch-to-buffer)
               (", j" . counsel-file-jump)
               (", k" . kill-buffer)
               (", o" . ace-window)
               ("M-d" . counsel-M-x)
               ("`" . counsel-M-x))))

(use-package counsel
  :defines org-agenda-mode-map
  :after org
  :bind ((:map org-agenda-mode-map
               ("`" . counsel-M-x)
               ("M-d" . counsel-M-x))))

(use-package css-mode
  :no-require t
  :custom ((css-indent-offset 2)))

(use-package desktop
  :custom ((desktop-base-file-name "emacs.desktop")
           (desktop-restore-eager 0)
           (desktop-lazy-idle-delay 2)
           (desktop-lazy-verbose nil))
  :hook ((desktop-after-read . desktop-auto-save-enable))
  :demand t
  :config (progn
            (setq desktop-dirname (car desktop-path))
            ;; TODO Move these variables to the respective
            ;; use-package.
            (dolist (variable '(command-history
                                compile-history
                                evil-ex-history
                                evil-ex-search-history
                                log-edit-comment-ring
                                minibuffer-history
                                read-expression-history
                                shell-command-history))
              (add-to-list 'desktop-globals-to-save variable))
            (setq desktop-globals-to-save
                  (cl-set-difference desktop-globals-to-save
                                     '(register-alist)))))

(use-package desktop
  :defer 2
  :config (progn
            (desktop-save-mode)))

(use-package diff
  ;; We use :after diff here, which is a bit weird, as :custom will
  ;; autoload diff otherwise, as diff-switches is an autoloaded defvar.
  :after diff
  :no-require t
  :custom ((diff-switches "-u")))

(use-package dired
  ;; Same as for diff, we use :after dired.
  :after dired
  :custom ((dired-dwim-target t)
           (dired-listing-switches "--si -al")
           (dired-recursive-copies 'always)
           (dired-recursive-deletes 'always))
  :bind (:map dired-mode-map
              ("e" . now-dired-ediff-files)))

(use-package dired-aux
  :no-require t
  :custom ((dired-isearch-filenames 'dwim)))

(use-package dired-x
  :after dired)

(use-package disp-table
  :config (progn
            (defface wrap-glyph
              '((((min-colors 16) (class color))
                 :foreground "blue")
                (t
                 :inverse-video t))
              "Face for wrap glyph."
              :group 'basic-faces)
            (set-display-table-slot standard-display-table 'wrap #x00a0)
            (set-display-table-slot standard-display-table 'selective-display
                                    (vector (make-glyph-code #x2026)))
            (set-display-table-slot standard-display-table 'vertical-border 0)))

(use-package display-fill-column-indicator
  :hook (((prog-mode text-mode) . display-fill-column-indicator-mode)))

(use-package docfold
  :hook ((prog-mode . docfold-minor-mode)))

(use-package ediff
  :no-require t
  :custom ((ediff-window-setup-function 'ediff-setup-windows-plain)
           (ediff-split-window-function 'split-window-horizontally)))

(use-package eglot
  :ensure t
  :custom ((eglot-ignored-server-capabilites '(:documentHighlightProvider))))

(use-package eldoc
  :diminish
  :custom ((eldoc-echo-area-use-multiline-p nil))
  :hook ((emacs-lisp-mode . eldoc-mode)))

(use-package electric
  :hook ((prog-mode . electric-quote-local-mode)))

(use-package elisp-mode
  :config (font-lock-add-keywords
           'emacs-lisp-mode
           `((,(concat "(" (eval-when-compile
                             (regexp-opt '("cl-assert"
                                           "cl-check-type"
                                           "error"
                                           "signal"
                                           "user-error"
                                           "warn")
                                         t))
                       "\\_>")
              (1 font-lock-keyword-face)))))

(use-package now-elisp-mode
  :hook ((emacs-lisp-mode . now-emacs-lisp-mode-hook)))

(use-package epa
  :after evil
  :defer t
  :config (progn
            (evil-make-overriding-map epa-key-list-mode-map nil)))

(use-package ffap
  :custom ((ffap-machine-p-known 'accept)))

(use-package files
  :custom ((make-backup-files nil)
           (require-final-newline t)
           (revert-without-query '("\\.log$")))
  :defer t
  :config (progn
            (setq insert-directory-program "a")))

(use-package files
  :after evil
  :bind (:map evil-motion-state-map
              (", W" . save-some-buffers)
              ))

(use-package find-func
  :defer t
  :config (progn
            (setq find-function-C-source-directory "~/Projects/emacs/src")))

;; TODO Customize
(use-package flycheck
  :disabled)

(use-package flyspell
  :hook ((message-mode . flyspell-mode)))

(use-package frame
  :custom ((blink-cursor-blinks 0)
           (default-frame-alist '((right-fringe . 0))))
  :config (blink-cursor-mode))

(defun now-window-size-change-function (_)
  (pcase (cdr (assoc 'geometry
                     (car (display-monitor-attributes-list))))
    ((and `(,_ ,_ ,width ,height) (guard (< width height)))
     (setq split-width-threshold nil))
    (_
     (setq split-width-threshold 160))))
(setq window-size-change-functions '(now-window-size-change-function))

(use-package json-mode
  :ensure t)

(use-package now-git
  :after evil
  :no-require t
  :bind ((:map evil-motion-state-map
               (", g" . now-git-grep))))

(use-package git-rebase
  :after evil
  :defer t
  :config (progn
            (evil-make-overriding-map git-rebase-mode-map nil)))

(use-package go-mode
  :ensure t
  :bind (("C-c C-c" . comment-region))
  :hook ((go-mode . now-go-mode-hook))
  :config (defun now-go-mode-hook ()
            (indent-tabs-mode)
            (setq-local fill-column 100)))

(use-package grep
  :custom ((grep-highlight-matches t)
           (grep-program "g")
           (grep-use-null-device nil)
           (grep-use-null-filename-separator t))
  :config (setq grep-regexp-alist
            `((,(concat "^\\(?:"
                        ;; Parse using NUL characters when `--null' is used.
                        ;; Note that we must still assume no newlines in
                        ;; filenames due to "foo: Is a directory." type
                        ;; messages.
                        "\\(?1:[^\0\n]+\\)\\(?3:\0\\)\\(?2:[0-9]+\\)\\(?::\\|\\(?4:\0\\)\\)"
                        "\\|"
                        ;; Fallback if `--null' is not used, use a tight regexp
                        ;; to handle weird file names (with colons in them) as
                        ;; well as possible.  E.g., use [1-9][0-9]* rather than
                        ;; [0-9]+ so as to accept ":034:" in file names.
                        "\\(?1:"
                        "\\(?:[a-zA-Z]:\\)?" ; Allow "C:..." for w32.
                        "[^\n:]+?[^\n/:]\\):[\t ]*\\(?2:[1-9][0-9]*\\)[\t ]*:"
                        "\\)")
               1 2
               ;; Calculate column positions (col . end-col) of first grep match on a line
               (,(lambda ()
                   (when grep-highlight-matches
                     (let* ((beg (match-end 0))
                            (end (save-excursion (goto-char beg) (line-end-position)))
                            (mbeg (text-property-any beg end 'font-lock-face grep-match-face)))
                       (when mbeg
                         (- mbeg beg)))))
                .
                ,(lambda ()
                   (when grep-highlight-matches
                     (let* ((beg (match-end 0))
                            (end (save-excursion (goto-char beg) (line-end-position)))
                            (mbeg (text-property-any beg end 'font-lock-face grep-match-face))
                            (mend (and mbeg (next-single-property-change mbeg 'font-lock-face nil end))))
                       (when mend
                         (- mend beg))))))
               nil nil
               (3 '(face nil display ":"))
               (4 '(face nil display ":")))
              ("^Binary file \\(.+\\) matches$" 1 nil nil 0 1))))

(use-package grep
  :after evil
  :no-require t
  :bind ((:map evil-motion-state-map
               (", G" . grep)))
  :config (progn
            (evil-make-overriding-map grep-mode-map nil)))

(use-package gxref
  :ensure t)

(use-package now-gxref
  :after evil
  :bind ((:map evil-motion-state-map
               (", E" . now-gxref-find-file))))

(use-package hide-mode-line
  :config (hide-mode-line-mode))

(use-package hide-mode-line
  :after evil
  :bind (:map evil-motion-state-map
              (", i" . hide-mode-line-show-mode-line)))

(use-package highlight-selected-window
  :config (highlight-selected-window-mode))

(use-package hl-line
  :hook (((Buffer-menu-mode
            arc-mode
            dired-mode
            org-agenda-mode
            tar-mode)
           . hl-line-mode)))

(use-package holidays
  :after holidays
  :custom ((holiday-general-holidays '((holiday-fixed 1 1 "New Year’s Day")
                                       (holiday-fixed 5 1 "International Worker’s Day")
                                       (holiday-fixed 6 6 "National Day of Sweden")
                                       (holiday-float 6 5 -1 "Midsummer’s Eve" 26)
                                       (holiday-fixed 12 31 "New Year’s Eve")))
           (holiday-christian-holidays '((holiday-easter-etc -2 "Good Friday")
                                         (holiday-easter-etc +1 "Easter Monday")
                                         (holiday-easter-etc +39 "Ascension Day")
                                         (holiday-easter-etc +49 "Pentecost")
                                         (holiday-fixed 12 24 "Christmas Eve")
                                         (holiday-fixed 12 25 "Christmas")
                                         (holiday-fixed 12 26 "Boxing Day")))
           (calendar-holidays (append holiday-general-holidays
                                      holiday-local-holidays
                                      holiday-christian-holidays
                                      holiday-other-holidays
                                      holiday-solar-holidays)))
  :evil-bind ((:map motion calendar-mode-map
                    ("H" . calendar-cursor-holidays))))

(use-package info
  :evil-bind ((:map motion Info-mode-map
                    ("C-i" . Info-history-forward)))
  :config (progn
            (add-to-list 'Info-additional-directory-list
                         (concat user-emacs-directory "info"))))

(use-package isearch
  :no-require t
  :custom ((search-whitespace-regexp "[ \t\r\n]+")))

(use-package ispell
  :no-require t
  :custom ((ispell-local-dictionary-alist
            '((nil "[[:alpha:]]" "[^[:alpha:]]" "['’]" nil ("-B") nil utf-8)
              ("en_US" "[[:alpha:]]" "[^[:alpha:]]" "['’]" nil ("-B") nil utf-8)
              ("en_GB-ise" "[[:alpha:]]" "[^[:alpha:]]" "['’]" nil ("-B") nil utf-8)
              ("sv_SE" "[[:alpha:]]" "[^[:alpha:]]" "['’]" nil ("-C") nil utf-8)))))

(use-package jka-cmpr-hook
  :no-require t
  :custom ((jka-compr-verbose . nil)))

(use-package js
  :no-require t
  :mode (("\\.jsx\\(inc\\)?\\'" . js-mode))
  :custom ((js-indent-level 2)))

(use-package magit
  :ensure t
  :no-require t
  :custom ((magit-repository-directories '(("~/Projects". 1)))))

(use-package magit-blame
  :defer t
  :config (setf (alist-get 'heading-format
                           (alist-get 'headings magit-blame-styles))
                "%-20a %C %.6H %s\n"))

(use-package magit-files
  :no-require t
  :after evil
  :bind ((:map evil-motion-state-map
               (", S" . magit-file-dispatch))))

(use-package magit-popup
  :no-require t
  :custom ((magit-popup-show-common-commands nil)))

(use-package magit-status
  :no-require t
  :after evil
  :bind ((:map evil-motion-state-map
               (", s" . magit-status))))

(use-package make-mode
  :no-require t
  :custom ((makefile-backslash-align nil)))

(use-package man
  :custom ((Man-notify-method 'pushy))
  :evil-bind ((:map normal Man-mode-map
                    ("q" . Man-quit))))

(use-package markdown-mode
  :ensure t
  :custom ((markdown-list-item-bullets '("•" "◦" "‣"))))

(use-package menu-bar
  :commands (menu-bar-mode)
  :defer t
  :config (progn
            (menu-bar-mode -1)))

(use-package message
  :defer t
  :custom ((message-citation-line-format "%N, %Y-%m-%d %R:\n")
           (message-citation-line-function 'message-insert-formatted-citation-line)
           (message-kill-buffer-on-exit t)
           (message-send-mail-function 'smtpmail-multi-send-it)
           (message-subscribed-addresses nil))
  :config (progn
            (define-abbrev message-mode-abbrev-table "br"
              "Best regards,\n  Nikolai"
              nil :system t :case-fixed t)
            (define-abbrev message-mode-abbrev-table "tbr"
              "Thank you and best regards,\n  Nikolai"
              nil :system t :case-fixed t)
            (define-abbrev message-mode-abbrev-table "tsn"
              "Thanks,\n  Nikolai"
              nil :system t :case-fixed t)))

(use-package minibuffer
  :no-require t
  :custom ((completions-format 'vertical)))

(use-package mule
  :custom ((default-input-method "rfc1345--"))
  :defer t
  :config (progn
            (quail-define-package
             "rfc1345--" "UTF-8" "m-" t
             "Unicode characters input method using a subset of RFC1345.
For example, “&a'” → “á”"
             nil t nil nil nil nil nil nil nil nil t)
            (require 'rfc1345 "quail/rfc1345")
            (let ((map (quail-map)))
              (quail-select-package "rfc1345--")
              (setf (nth 2 quail-current-package) map)
              (quail-define-rules
               ((append . t))
               ("&(-" ?\∈)
               ("&(/" ?\∉)
               ("&cb" ?\•)
               ("&:)" ?\☺)
               ("&:(" ?\☹)
               ("&<3" ?\❤)))))

(use-package mule-util
  :defer t
  :config (setq truncate-string-ellipsis "…"))

(use-package now-init
  :hook ((compilation-mode . now-do-not-show-trailing-whitespace)
         (message-mode . now-remove-continuation-fringe-indicator)
         (prog-mode . now-comment-auto-fill-only-comments)
         (tabulated-list-mode
          . now-tabulated-list-mode-use-global-glyphless-char-display)
         ((nxml-mode prog-mode text-mode) . now-set-fill-column-to-80)))

(use-package nxml-mode
  :custom ((nxml-char-ref-display-glyph-flag nil)
           (nxml-sexp-element-flag t)
           (nxml-slash-auto-complete-flag t))
  :mode ("\\.rng\\'"
         "\\.sch\\'"
         "\\.xsd\\'")
  :defer t
  :config (progn
            (define-abbrev nxml-mode-abbrev-table "s" ""
              'nxml-mode-skeleton-xsl-stylesheet
              :system t)
            (define-abbrev nxml-mode-abbrev-table "t" ""
              'nxml-mode-skeleton-xsl-template
              :system t)
            (define-skeleton nxml-mode-skeleton-xsl-stylesheet
              "Insert an XSL Stylesheet."
              nil
              "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" \n
              "<xsl:stylesheet version=\"1.0\" xmlns:xsl=\"http://www.w3.org/1999/XSL/Transform\">" \n
              "<xsl:output method=\"xml\" encoding=\"UTF-8\"/>" \n
              _ \n
              "</xsl:stylesheet>" >)
            (define-skeleton nxml-mode-skeleton-xsl-template
              "Insert an XSL Template."
              nil
              "<xsl:template match=\"" _ "\">" \n
              "</xsl:template>" >)))

(use-package org
  :custom ((org-directory "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents")
           (org-agenda-deadline-faces '((0.0 . default)))
           (org-agenda-text-search-extra-files '(agenda-archives))
           (org-archive-location "%s.archive.org::datetree/")
           (org-catch-invisible-edits 'smart)
           (org-columns-default-format "%80ITEM(Task) %7Effort{:} %7CLOCKSUM(Clocked)")
           (org-columns-ellipses "…")
           (org-edit-src-persistent-message nil)
           (org-enforce-todo-dependencies t)
           (org-global-properties '(("Effort_ALL" .
                                     "0:15 0:30 1:00 2:00 4:00 8:00 0:00")))
           (org-hide-emphasis-markers t)
           (org-highlight-sparse-tree-matches nil)
           (org-link-frame-setup '((vm . vm-visit-folder)
                                   (file . find-file)))
           (org-log-done 'time)
           (org-log-into-drawer t)
           (org-log-redeadline t)
           (org-log-reschedule t)
           (org-loop-over-headlines-in-active-region 'start-level)
           (org-outline-path-complete-in-steps nil)
           (org-reverse-note-order t)
           (org-refile-allow-creating-parent-nodes 'confirm)
           (org-refile-target-verify-function 'now-org-refile-target-verify)
           (org-refile-targets '((org-agenda-files . (:maxlevel . 9))))
           (org-refile-use-outline-path 'file)
           (org-src-window-setup 'current-window)
           (org-todo-keyword-faces '(("DLGT" . org-delegated)
                                     ("HOLD" . org-hold)
                                     ("NEXT" . org-next)
                                     ("WAIT" . org-waiting)))
           (org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                                (sequence "WAIT(w@/@)" "DLGT(g@/!)" "HOLD(h)" "|"
                                          "NIXD(c@/!)")
                                (type "CALL")
                                (type "CHAT")))
           (org-treat-S-cursor-todo-selection-as-state-change nil)
           (org-yank-adjusted-subtrees t))
  :bind ((:map narrow-map
               ("T" . now-org-narrow-to-subtree-and-show-todo-tree)))
  :evil-bind ((:map motion org-mode-map
                    ("<RET>" . org-cycle))
              (:map normal org-mode-map
                    (", i" . org-clock-in)
                    (", o" . org-clock-out)
                    (", P" . org-set-property)
                    (", T" . org-set-effort)
                    (", t" . org-todo)))
  :config (progn
            (defface org-delegated nil
              "Face used for DELEGATED todo keyword."
              :group 'org-faces)
            (defface org-hold nil
              "Face used for HOLD todo keyword."
              :group 'org-faces)
            (defface org-next nil
              "Face used for NEXT todo keyword."
              :group 'org-faces)
            (defface org-waiting nil
              "Face used for WAIT todo keyword."
              :group 'org-faces)
            (setq org-agenda-files (concat
                                    (file-name-as-directory org-directory)
                                    "agenda.lst")
                  org-default-notes-file (concat
                                          (file-name-as-directory org-directory)
                                          "refile.org"))
            (setf (cdr (assq 'state org-log-note-headings))
                  "State %s from %S %t")
            (add-to-list 'org-file-apps '(directory . emacs))
            (org-clock-persistence-insinuate)))

(use-package now-org
  :hook ((org-after-todo-state-change
          . now-org-adjust-properties-after-todo-state-change)
         (org-insert-heading . now-org-insert-heading-add-log-note)))

(use-package org-agenda
  :after org
  :custom ((org-agenda-clockreport-parameter-plist '(:link t
                                                           :maxlevel 5
                                                           :fileskip0 t
                                                           :compact t
                                                           :narrow 80))
           (org-agenda-compact-blocks t)
           (org-agenda-custom-commands
            `(("A" "Archivals"
               ((tags "-REFILE/DONE|NIXD"
                      ((org-agenda-overriding-header "Archivals")
                       (org-agenda-skip-function 'now-org-agenda-skip-unless-archival)
                       (org-tags-match-list-sublevels nil)))))))
           (org-agenda-deadline-leaders '("Deadline:  " "In %d days: " "%d days ago: "))
           (org-agenda-diary-file (concat (file-name-as-directory org-directory)
                                          "personal.org"))
           (org-agenda-dim-blocked-tasks nil)
           (org-agenda-fontify-priorities t)
           (org-agenda-insert-diary-strategy 'date-tree-last)
           (org-agenda-tags-todo-honor-ignore-options t)
           (org-agenda-log-mode-items '(clocked closed state))
           (org-agenda-prefix-format '((agenda . "% i%-11c%?-12t% s")
                                       (timeline . "% s")
                                       (todo . "% i%-11c")
                                       (tags . "% i%-11c")
                                       (search . "% i%-11c")))
           (org-agenda-sorting-strategy
            '((agenda habit-down time-up priority-down effort-up timestamp-down
                      category-keep)
              (todo priority-down effort-up timestamp-down category-up)
              (tags priority-down effort-up timestamp-down category-up)
              (search category-up)))
           (org-agenda-sticky t)
           (org-agenda-time-leading-zero t)
           (org-agenda-use-time-grid nil))
  :bind ((:map org-agenda-mode-map
               ("n" . org-agenda-next-item)
               ("p" . org-agenda-previous-item))))

(use-package org-agenda
  :after evil
  :bind ((:map evil-motion-state-map
               (", a" . org-agenda))))

(use-package org-archive
  :custom ((org-archive-save-context-info '(time olpath category todo itags))))

(use-package org-capture
  :after org
  :custom ((org-capture-templates '(("c" "Chat" entry
                                     (file "")
                                     "* CHAT with %? :CHAT:\n%U"
                                     :clock-in t :clock-resume t)
                                    ("j" "Journal Entry (Today)" entry
                                     (file+olp+datetree org-agenda-diary-file)
                                     "* %?\n% %U\n")
                                    ("J" "Journal Entry (Date)" entry
                                     (file+olp+datetree org-agenda-diary-file)
                                     "* %?\n%t %U\n"
                                     :time-prompt t)
                                    ("p" "Phone Call" entry
                                     (file "")
                                     "* CALL %? :CALL:\n%U"
                                     :clock-in t :clock-resume t)
                                    ("t" "Todo" entry
                                     (file "")
                                     "* TODO %?\n%U\n%i"
                                     :clock-in t :clock-resume t)
                                    ("T" "Annotated Todo" entry (file "")
                                     "* TODO %?\n%U\n%i\n%a"
                                     :clock-in t :clock-resume t)))))

(use-package org-capture
  :after evil
  :bind ((:map evil-motion-state-map
               (", O" . org-capture))))

(use-package org-clock
  :after org
  :custom ((org-clock-in-switch-to-state 'now-org-clock-in-switch-to-state)
           (org-clock-out-remove-zero-time-clocks t)
           (org-clock-out-when-done '("HOLD" "WAIT" "DLGT" "DONE" "NIXD"))
           (org-clock-persist t)
           (org-clock-persist-query-resume nil)
           (org-clock-report-include-clocking-task t)))

(use-package org-colview
  :bind ((:map org-columns-map
               (", t" . org-columns-todo)
               ("b" . backward-char)
               ("h" . backward-char)
               ("j" . now-org-columns-forward-line)
               ("k" . now-org-columns-backward-line)
               ("s" . now-org-columns-forward-char)
               ("w" . now-org-columns-forward-char))))

(use-package org-faces
  :after org
  :custom ((org-priority-faces '((?A org-priority-a)
                                 (?B org-priority-b)
                                 (?C org-priority-c))))
  :config (progn
            (defface org-priority-a nil
              "Face used for priority A."
              :group 'org-faces)
            (defface org-priority-b nil
              "Face used for priority B."
              :group 'org-faces)
            (defface org-priority-c nil
              "Face used for priority C."
              :group 'org-faces)))

(use-package org-id
  :after org
  :custom ((org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)))

(use-package org-mobile
  :after org
  :custom ((org-mobile-agendas nil)
           (org-mobile-directory "/scp:now@disu.se:Sites/dav/")
           (org-mobile-force-id-on-agenda-items nil)
           (org-mobile-inbox-for-pull (concat
                                       (file-name-as-directory org-directory)
                                       "refile.org"))))

(use-package package
  :no-require t
  :custom ((package-quickstart t)))

(use-package page
  :no-require t
  :config (put 'narrow-to-page 'disabled nil))

(use-package paredit
  :ensure t
  :diminish paredit-mode
  :evil-bind ((:map motion paredit-mode-map
                    ("g B" . evil-paredit-backward-up)
                    ("g W" . evil-paredit-forward-up)
                    ("g b" . evil-paredit-backward)
                    ("g w" . evil-paredit-forward))
              (:map normal paredit-mode-map
                    ("( <RET>" . paredit-split-sexp)
                    ("( J" . paredit-join-sexps)
                    ("( R" . paredit-raise-sexp)
                    ("( S" . paredit-splice-sexp-killing-backward)
                    ("( W" . paredit-wrap-round)
                    ("( s" . paredit-splice-sexp)
                    ("D" . paredit-kill)
                    ("X" . paredit-backward-delete)
                    ("x" . paredit-forward-delete)
                    ("( (" . evil-paredit-backward-slurp-sexp)
                    ("( {" . evil-paredit-backward-barf-sexp)
                    (") )" . evil-paredit-forward-slurp-sexp)
                    (") }" . evil-paredit-forward-barf-sexp)))
  :hook (((emacs-lisp-mode
            eval-expression-minibuffer-setup
            lisp-interaction-mode
            lisp-mode)
           . paredit-mode)))

(use-package paren
  :no-require t
  :custom ((show-paren-delay 0))
  :config (progn
            (show-paren-mode)))

(use-package project
  :ensure t)

(use-package recentf
  :no-require t
  :custom ((recentf-max-saved-items 50)
           (recentf-save-file (concat user-emacs-directory "recentf"))))

(use-package replace
  :no-require t
  :custom ((replace-lax-whitespace t)))

(use-package rnc-mode
  :defer t
  :config (progn
            (define-abbrev rnc-mode-abbrev-table "a" ""
              'now-rnc-mode-skeleton-attribute :system t)
            (define-skeleton now-rnc-mode-skeleton-attribute
              "Insert an attribute definition."
              "Prefix: "
              "div {" \n
              str
              '(setq v1 (skeleton-read "Element name: "))
              '(setq v2 (skeleton-read "Attribute name: "))
              "." v1 ".attributes &= " str "." v1 ".attributes." v2 \n
              str "." v1 ".attributes." v2 " = attribute " v2 " { " _ " }" \n
              "}" >)
            (define-abbrev rnc-mode-abbrev-table "d" ""
              'now-rnc-mode-skeleton-div :system t)
            (define-skeleton now-rnc-mode-skeleton-div
              "Insert a div."
              nil
              "div {" \n
              _ \n
              "}" >)
            (define-abbrev rnc-mode-abbrev-table "e" ""
              'now-rnc-mode-skeleton-element :system t)
            (define-skeleton now-rnc-mode-skeleton-element
              "Insert an element definition."
              "Prefix: "
              "div {" \n
              str
              '(setq v1 (skeleton-read "Element name: "))
              "." v1 " = element " v1 " { " str "." v1 ".attributes, "
              str "." v1 ".content }" "\n"
              \n
              str "." v1 ".attributes = " _ "\n"
              \n
              (- rnc-indent-level) str "." v1 ".content = " \n
              "}" >)))

(use-package rng-loc
  :defer t
  :config (progn
            (add-to-list 'rng-schema-locating-files
                         (concat user-emacs-directory
                                 "etc/schema/schemas.xml"))))

(use-package ruby-mode
  :evil-bind ((:map normal ruby-mode-map
                    (", t" . ruby-find-other-file)
                    (", M" . ruby-run-test-at-line)))
  :hook ((ruby-mode . now-ruby-mode-hook))
  :config (progn
            (define-abbrev ruby-mode-abbrev-table "d" "" 'ruby-skeleton-def
              :system t)
            (define-skeleton ruby-skeleton-def
              "Insert a method definition."
              "Method name and argument list: "
              "def " str \n
              _ \n
              "end" >)
            (define-abbrev ruby-mode-abbrev-table "tlc" ""
              'ruby-skeleton-top-level-class
              :system t)
            (define-skeleton ruby-skeleton-top-level-class
              "Insert a top-level class."
              nil
              "# -*- coding: utf-8 -*-" \n
              \n
              "class " (ruby-file-name-to-module-name) \n
              _ \n
              "end" >)
            (define-abbrev ruby-mode-abbrev-table "tlm" ""
              'ruby-skeleton-top-level-module
              :system t)
            (define-skeleton ruby-skeleton-top-level-module
              "Insert a top-level module."
              nil
              "# -*- coding: utf-8 -*-" \n
              \n
              "module " (ruby-file-name-to-module-name) \n
              _ \n
              "end" >)
            (define-abbrev ruby-mode-abbrev-table "tle" ""
              'ruby-skeleton-top-level-expectations :system t)
            (define-skeleton ruby-skeleton-top-level-expectations
              "Insert top-level expectations."
              nil
              "# -*- coding: utf-8 -*-" \n
              \n
              "Expectations do" \n
              "expect " _ " do" \n
               _ \n
              "end" \n
              "end" >)
            (defun now-ruby-mode-hook ()
              (setq-local compile-command "rake -s ")
              (setq-local paragraph-start
                          (rx (| ?\f
                                 (: (* (in ?\  ?\t)) eol)
                                 (: (* (in ?\  ?\t))
                                    ?#
                                    (* (in ?\  ?\t))
                                    ?@
                                    (+ (in alpha))
                                    (in ?\  ?\t)))))
              (setq-local adaptive-fill-function
                          'now-ruby-mode-adaptive-fill-function))))

(use-package sbt-mode
  :ensure t
  :config (progn
            (defun now-sbt-mode-hook ()
              (setq-local comint-use-prompt-regexp nil)
              (setq-local compilation-disable-input nil))
            (add-hook 'sbt-mode-hook 'now-sbt-mode-hook -20)))

(use-package scala-mode
  :ensure t)

(use-package now-scala-mode
  :no-require t
  :hook ((scala-mode . now-scala-mode-setup))
  :config (define-auto-insert
            'scala-mode
            #'now-scala-mode-auto-insert-mode-skeleton))

(use-package scroll-bar
  :commands (scroll-bar-mode)
  :defer t
  :config (progn
            (scroll-bar-mode -1)))

(use-package sed-mode
  :ensure t
  :hook ((sed-mode . now-sed-mode-hook))
  :config (defun now-sed-mode-hook ()
            (setq-local smie-indent-basic 2)))

(use-package semantic/format
  :no-require t
  :config (progn
            (setq-default semantic-function-argument-separator ", ")))

(use-package server
  :unless noninteractive
  :no-require
  :hook (after-init . server-start))

(use-package sh-script
  :custom ((sh-indentation 2)
           (sh-basic-offset 2))
  :defer t
  :config (progn
            (add-to-list 'sh-alias-alist '(@SHELL@ . sh))
            (add-to-list 'sh-alias-alist '(@ZSHELL@ . zsh))))

(use-package shr
  :no-require t
  :custom ((shr-use-fonts nil)
           (shr-width nil)
           (shr-bullet "• ")))

(use-package simple
  :diminish auto-fill-function
  :no-require t
  :custom ((completion-show-help nil))
  :hook (((prog-mode text-mode) . auto-fill-mode)
         (nxml-mode . turn-off-auto-fill))
  :defer nil
  :config (progn
            (column-number-mode)
            (indent-tabs-mode -1)))

(use-package simple
  :no-require t
  :after evil
  :bind ((:map evil-motion-state-map
               (", c" . shell-command)
               (", n" . next-error)
               (", p" . previous-error))))

(use-package smerge-mode
  :custom ((smerge-auto-leave nil)
           (smerge-command-prefix "\C-cv")))

(use-package smie
  :defer t
  :config (progn
            (advice-add 'smie-auto-fill :around 'now-smie-auto-fill)))

(use-package solar
  :no-require t
  :custom ((calendar-time-display-form '(24-hours ":" minutes (if time-zone " ")
                                                  time-zone))
           (calendar-latitude 57.708870)
           (calendar-longitude 11.97456)
           (calendar-location-name "Göteborg")))

(use-package smtpmail
  :no-require t
  :custom ((smtpmail-queue-dir "~/Maildir/.Queue/cur")))

(use-package smtpmail-multi
  :ensure t
  :no-require t
  :custom ((smtpmail-multi-accounts '((disuse . ("now@disu.se"
                                                 "disu.se"
                                                 587
                                                 nil nil nil nil nil))))
           (smtpmail-multi-associations '(("now@disu.se" disuse)))))

(use-package swiper
  :after evil
  :bind ((:map evil-motion-state-map
               (", /" . swiper))))

(use-package tar-mode
  :after evil
  :evil-bind ((:map motion tar-mode-map
                    ("<RET>" . tar-extract)
                    ("j" . tar-next-line)
                    ("k" . tar-previous-line)
                    ("h" . evil-backward-char)))
  :config (progn
            (evil-make-overriding-map tar-mode-map 'normal)))

(use-package term/xterm
  :defer t
  :config (progn
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
            (set-terminal-parameter nil 'background-mode 'light)))

(use-package tool-bar
  :commands (tool-bar-mode)
  :defer t
  :config (progn
            (tool-bar-mode -1)))

(use-package tramp-sh
  :no-require t
  :custom ((tramp-copy-size-limit nil)))

(use-package typescript-mode
  :ensure t
  :custom ((typescript-indent-columns nil))
  :bind (:map typescript-mode-map
              ("*" . c-electric-star)))

(use-package typescript-mode
  :no-require t
  :after typescript-mode
  :config (dolist (entry '(typescript-tsc
                           typescript-tsc-pretty
                           typescript-tslint
                           typescript-nglint-error
                           typescript-nglint-warning))
            (setq compilation-error-regexp-alist
                  (delq entry compilation-error-regexp-alist))))

(use-package typescriptreact-mode
  :no-require t
  )
(use-package undo-tree
  :disabled
  :custom ((undo-tree-mode-lighter nil)
           (undo-tree-visualizer-timestamps t))
  :config (global-undo-tree-mode))

(use-package undo-tree
  :disabled
  :after evil
  :bind (:map evil-motion-state-map
              (", u" . undo-tree-visualize)
              ("U" . undo-tree-redo)))

(use-package vc-git
  :defer t
  :config (progn
            (defun vc-git-mode-line-string (_) "")))

(use-package vc-hooks
  :defer t
  :custom ((vc-handled-backends '(Git))))

(use-package view
  :evil-bind ((:map normal view-mode-map
                    ("q" . quit-window))))

(use-package visual-fill-column
  :ensure t
  :hook ((message-mode . visual-fill-column-mode)))

(use-package whitespace
  :diminish whitespace-mode
  :custom ((whitespace-style '(face
                               trailing
                               empty
                               indentation
                               space-before-tab))))

(use-package xdisp
  :no-require t
  :custom ((show-trailing-whitespace t))
  :config (progn
            (setq overlay-arrow-string "►")))

(use-package xref
  :defer nil
  :bind ((:map xref--xref-buffer-mode-map
               ("q" . quit-window)))
  :config (progn
	    (require 'xref)
            (add-to-list 'xref-backend-functions 'gxref-xref-backend)))

(use-package xref
  :functions (evil-put-command-property)
  :no-require t
  :after evil
  :bind ((:map evil-motion-state-map
               ("C-]" . xref-find-definitions)
               ("g C-]" . xref-find-references))
         (:map evil-normal-state-map
               ("C-t" . xref-pop-marker-stack)))
  :config (progn
            (evil-put-command-property 'xref-find-definitions :jump t)))

(defun light-or-dark-mode ()
  (interactive)
  (customize-set-variable
   'frame-background-mode
   (if (= (do-applescript
           "tell application \"System Events\"
            if get dark mode of appearance preferences then
              1
            else
              0
            end if
          end tell") 1)
       'dark
     'light))
  (customize-set-variable 'custom-enabled-themes custom-enabled-themes))

;; TODO Move these to the relevant use-package
(declare-function evil-mode "evil-core")
(evil-mode)
(load-theme 'now t)

(defun desktop-value-to-string (value)
  "Convert VALUE to a string that when read evaluates to the same value.
Not all types of values are supported."
  (declare-function desktop--v2s "desktop")
  (let* ((print-escape-newlines t)
	 (print-length nil)
	 (print-level nil)
         (print-circle t)
	 (float-output-format nil)
	 (quote.sexp (desktop--v2s value))
	 (quote (car quote.sexp))
	 (print-quoted t)
	 (txt (prin1-to-string (cdr quote.sexp))))
    (if (eq quote 'must)
	(concat "'" txt)
      txt)))

(defun now-bug-reference-fontify-around (next &rest args)
  (let ((case-fold-search nil))
    (apply next args)))

(advice-add 'bug-reference-fontify :around 'now-bug-reference-fontify-around)
