;; -*- lexical-binding: t; -*-

(add-to-list 'load-path (concat user-emacs-directory "site-lisp"))
(require 'userloaddefs)

;(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/")
             'append)

(setq ring-bell-function 'ignore)
(setq gc-cons-threshold 20000000)
(setq process-connection-type nil)

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

(global-set-key [f1] nil)
(global-set-key [f2] nil)
(global-set-key [f3] nil)
(global-set-key [f4] nil)

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

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
           (evil-move-cursor-back nil)
           (evil-shift-round nil)
           (evil-shift-width 2)
           (evil-symbol-word-search t)
           (evil-want-abbrev-expand-on-insert-exit nil))
  :bind ((:map evil-insert-state-map
               ("C-d" . evil-normal-state))
         (:map evil-motion-state-map
               (",")
               ("<DEL>" . evil-scroll-page-up)
               ("<SPC>" . evil-scroll-page-down)
               (", B" . buffer-menu)
               (", b" . switch-to-buffer)
               (", e" . find-file)
               (", k" . kill-buffer)
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
            (ivy-mode 1)))

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
            (setq-default calc-group-char " "
                          calc-gnuplot-default-device "dumb"
                          calc-show-banner nil)
            (defmath circleSegmentArea (r n)
              (interactive 2 "csa")
              (let ((θ (* 180 (/ n (* 2 pi r)))))
                (- (* (/ θ 180) pi (^ r 2)) (* (sin θ) (cos θ) (^ r 2)))))))

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
            (let ((common-prettify-symbols
                   '(("--" . (?− (Br . Bl) ?−))
                     ("-" . ?−)
                     ("!" . ?¬)
                     ;; ("~" . ??)
                     (" * " . (?\s (Br . Bl) ?\s (Bc . Bc) ?×))
                     ;; ("/" . ?÷)
                     (" / " . (?\s (Br . Bl) ?\s (Bc . Bc) ?/))
                     (" % " . (?\s (Br . Bl) ?\s (Bc . Bc) ?%))
                     (" + " . (?\s (Br . Bl) ?\s (Bc . Bc) ?+))
                     (" - " . (?\s (Br . Bl) ?\s (Bc . Bc) ?−))
                     ;; ("<<" . ??)
                     ;; (">>" . ??)
                     ("<=" . ?≤)
                     (">=" . ?≥)
                     ("==" . ?=) ; ?≡
                     ("!=" . ?≠)
                     (" & " . (?\s (Br . Bl) ?∩ (Br . Bl) ?\s))
                     ("^" . ?⊕)
                     (" ^ " . (?\s (Br . Bl) ?⊕ (Br . Bl) ?\s))
                     (" | " . (?\s (Br . Bl) ?∪ (Br . Bl) ?\s))
                     ("&&" . ?∧)
                     ("||" . ?∨)
                     ;; ("?" . ??)
                     ;; (":" . ??)
                     ("=" . ?←)
                     ("*=" . (?← (cc cc 0 80) ?×))
                     ("/=" . (?← (cc cc 0 80) ?÷))
                     ("%=" . (?← (cc cc 0 80) ?%))
                     ("+=" . (?← (cc cc 0 75) ?+))
                     ("-=" . (?← (cc cc 0 80) ?−))
                     ;; ("<<=" . ??)
                     ;; (">>=" . ??)
                     ("&=" . (?← (cc cc 0 75) ?∩))
                     ("^=" . (?← (cc cc 0 75) ?⊕))
                     ("|=" . (?← (cc cc 0 75) ?∪))
                     ;; ("..." . ?…)
                     )))
              (c-add-style "now-c-style"
                           `("linux"
                             (c-hanging-braces-alist . ((brace-entry-open)
                                                        (brace-list-open after)
                                                        (brace-list-close before)
                                                        (class-close before)
                                                        (class-open after)
                                                        (substatement-open after)
                                                        (block-close before)))
                             (c-hanging-colons-alist . ((case-label after)
                                                        (label after)))
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
                                                        (: bol ?\f))))
                             (prettify-symbols-compose-predicate
                              . now-c-mode-prettify-symbols-compose-p)
                             (prettify-symbols-alist
                              . (,@common-prettify-symbols
                                 ;; TODO Not too sure about these.  This
                                 ;; list will never be complete.
                                 ("[0]" . ?₀)
                                 ("[1]" . ?₁)
                                 ("[2]" . ?₂)
                                 ("[3]" . ?₃)
                                 ("[4]" . ?₄)
                                 ("[5]" . ?₅)
                                 ("[6]" . ?₆)
                                 ("[i]" . ?ᵢ)
                                 ("[j]" . ?ⱼ)
                                 ("[k]" . ?ₖ)
                                 ("[m]" . ?ₘ)
                                 ("[n]" . ?ₙ)
                                 ("[i++]" . (?ᵢ (Br . Bl) ?₊ (Br . Bl) ?₊))
                                 ("[j++]" . (?ⱼ (Br . Bl) ?₊ (Br . Bl) ?₊))
                                 ("[k++]" . (?ₖ (Br . Bl) ?₊ (Br . Bl) ?₊))
                                 ("[-1]" . (?₋ (Br . Bl) ?₁))
                                 ("[i - 1]" . (?ᵢ (Br . Bl) ?₋ (Br . Bl) ?₁))
                                 ("[j - 1]" . (?ⱼ (Br . Bl) ?₋ (Br . Bl) ?₁))
                                 ("[k - 1]" . (?ₖ (Br . Bl) ?₋ (Br . Bl) ?₁))
                                 ("[m - 1]" . (?ₘ (Br . Bl) ?₋ (Br . Bl) ?₁))
                                 ("[n - 1]" . (?ₙ (Br . Bl) ?₋ (Br . Bl) ?₁))
                                 ("->" . ?→)
                                 ;; TODO Is this one really worth it?
                                 ("*" . ?∗)
                                 ("&" . ?↑)
                                 ("NULL" . ?∅)
                                 ;; ("unsigned char" . ?ℕᶜ)
                                 ;; ("unsigned short" . ?ℕₛ)
                                 ;; ("unsigned int" . ?ℕᵢ)
                                 ;; ("unsigned long" . ?ℕ)
                                 ;; ("unsigned long long" . ?ℕₗ)
                                 ;; ("uintmax_t" . ?ℕ₊)
                                 ;; ("char" . ?ℤᶜ)
                                 ;; ("short" . ?ℤₛ)
                                 ;; ("int" . ?ℤᵢ)
                                 ;; ("long" . ?ℤ)
                                 ;; ("long long" . ?ℤₗ)
                                 ;; ("int32_t" . (?ℤ (Br . Bl) ?₃ ( Br . BL) ?₂)
                                 ))))
              (c-add-style "now-java-style"
                           '("java"
                             (c-offsets-alist . ((arglist-intro . ++)))
                             (prettify-symbols-alist . common-prettify-symbols)
                             (whitespace-line-column . 120))))
            (defun now-c-mode-hook ()
              (setq-local adaptive-fill-function
                          'now-c-mode-adaptive-fill-function)
              (setq-local docfold-set-up-overlay
                          'docfold-c-set-up-overlay))))

(use-package now-cc-mode
  :bind ((:map c-mode-base-map
               ([remap c-fill-paragraph] . now-c-fill-paragraph)))
  :hook ((c-mode . now-c-mode-hook)
         (c-mode . now-c-auto-newline-mode)))

(use-package company
  :ensure t
  :diminish
  :defer 1
  :custom ((company-idle-delay .175)
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
            (global-company-mode)))

(use-package company-dabbrev
  :no-require t
  :custom ((company-dabbrev-downcase nil)
           (company-dabbrev-ignore-case nil)))

(use-package compile
  :custom ((compilation-context-lines 0)
           (compilation-disable-input t)
           (compilation-error-regexp-alist '(maven
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
            (push `(maven
                    ,(rx bol
                         (| (group-n 5 (: "[" (| "INFO" "info") "]"))
                            (: "[" (| "ERROR" "error"
                                      (group-n 4 (| (: "WARN" (? "ING"))
                                                    "warn"))) "] "
                               (? "[" (or "Warn" "Error") "] ")
                               (group-n 1
                                        (* (in (?0 . ?9)))
                                        (not (in (?0 . ?9) ?\n))
                                        (*? (| (not (in ?\n ?\s ?:))
                                               (: ?\s (not (in ?- ?/ ?\n)))
                                               (: ?: (not (in ?\s ?\n))))))
                               ":"
                               (| (: "["
                                     (group-n 2 (+ (in (?0 . ?9))))
                                     ","
                                     (group-n 3 (+ (in (?0 . ?9))))
                                     "]")
                                  (: (group-n 2 (+ (in (?0 . ?9)))) ":"
                                     (? (group-n 3 (+ (in (?0 . ?9)))) ":")))
                               " ")))
                    now-compilation-maven-file
                    2 3 (4 . 5) nil (1 (funcall 'now-compilation-maven-highlight)))
                  compilation-error-regexp-alist-alist)
            (defun now-compilation-maven-file ()
              (let ((s (match-string-no-properties 1)))
                (when s
                  (if (file-exists-p s)
                      s
                    (let ((java-1 (format "%s.java" s))
                          (scala-1 (format "%s.scala" s))
                          (java-2 (format "%s.java"
                                          (replace-regexp-in-string "\\." "/" s)))
                          (scala-2 (format "%s.scala"
                                           (replace-regexp-in-string "\\." "/" s))))
                      (cl-find-if
                       'file-exists-p
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
                        (format "src/test/scala/%s" scala-2))))))))
            (defun now-compilation-maven-highlight ()
              (let ((type (compilation-type '(4 . 5))))
                (compilation--note-type type)
                (symbol-value (aref [compilation-info-face
                                     compilation-warning-face
                                     compilation-error-face]
                                    type))))))

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
         ("C-x C-f" . counsel-find-file)))

(use-package counsel
  :after evil
  :bind ((:map evil-motion-state-map
               (", D" . counsel-dired-jump)
               (", F" . counsel-git-grep)
               (", J" . counsel-git)
               (", e" . counsel-find-file)
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
               (", e" . counsel-find-file)
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
           (desktop-restore-eager 5)
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

(use-package dired
  :after evil
  :bind (:map evil-motion-state-map
              (", d" . dired)))

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
            (set-display-table-slot standard-display-table 'wrap
                                    (make-glyph-code #x21A9 'wrap-glyph))
            (set-display-table-slot standard-display-table 'selective-display
                                    (vector (make-glyph-code #x2026)))
            (set-display-table-slot standard-display-table 'vertical-border 0)))

(use-package docfold
  :hook ((prog-mode . docfold-minor-mode)))

(use-package ediff
  :no-require t
  :custom ((ediff-window-setup-function 'ediff-setup-windows-plain)
           (ediff-split-window-function 'split-window-horizontally)))

(use-package eldoc
  :diminish
  :hook ((emacs-lisp-mode . eldoc-mode)))

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
              (", w" . save-buffer)))

(use-package find-func
  :defer t
  :config (progn
            (setq find-function-C-source-directory "~/Projects/emacs/src")))

;; TODO Customize
(use-package flycheck
  :ensure t)

(use-package flyspell
  :hook ((message-mode . flyspell-mode)))

(use-package frame
  :custom ((blink-cursor-blinks 0)))

(defun now-window-size-change-function (worf)
  (pcase (cdr (assoc 'geometry
                     (car (display-monitor-attributes-list))))
    ((and `(,_ ,_ ,width ,height) (guard (< width height)))
     (setq split-width-threshold nil))
    (_
     (setq split-width-threshold 240))))
(setq window-size-change-functions '(now-window-size-change-function))

(use-package git-rebase
  :after evil
  :defer t
  :config (progn
            (evil-make-overriding-map git-rebase-mode-map nil)))

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

(use-package now-git
  :after evil
  :no-require t
  :bind ((:map evil-motion-state-map
               (", g" . now-git-grep))))
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

(use-package indent
  :no-require t
  :custom ((indent-tabs-mode nil)))

(use-package info
  :evil-bind ((:map motion Info-mode-map
                    ("C-i" . Info-history-forward)))
  :config (progn
            (add-to-list 'Info-additional-directory-list
                         (concat user-emacs-directory "info"))))

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
  :ensure t)

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
            (quail-define-rules
             ("&NS" ?\ )
             ("&!I" ?\¡)
             ("&Ct" ?\¢)
             ("&Pd" ?\£)
             ("&Cu" ?\¤)
             ("&Ye" ?\¥)
             ("&BB" ?\¦)
             ("&SE" ?\§)
             ("&':" ?\¨)
             ("&Co" ?\©)
             ("&-a" ?\ª)
             ("&<<" ?\«)
             ("&NO" ?\¬)
             ("&--" ?\­)
             ("&Rg" ?\®)
             ("&'m" ?\¯)
             ("&DG" ?\°)
             ("&+-" ?\±)
             ("&2S" ?\²)
             ("&3S" ?\³)
             ("&''" ?\´)
             ("&My" ?\µ)
             ("&PI" ?\¶)
             ("&.M" ?\·)
             ("&'," ?\¸)
             ("&1S" ?\¹)
             ("&-o" ?\º)
             ("&>>" ?\»)
             ("&14" ?\¼)
             ("&12" ?\½)
             ("&34" ?\¾)
             ("&?I" ?\¿)
             ("&A!" ?\À)
             ("&A'" ?\Á)
             ("&A>" ?\Â)
             ("&A?" ?\Ã)
             ("&A:" ?\Ä)
             ("&AA" ?\Å)
             ("&AE" ?\Æ)
             ("&C," ?\Ç)
             ("&E!" ?\È)
             ("&E'" ?\É)
             ("&E>" ?\Ê)
             ("&E:" ?\Ë)
             ("&I!" ?\Ì)
             ("&I'" ?\Í)
             ("&I>" ?\Î)
             ("&I:" ?\Ï)
             ("&D-" ?\Ð)
             ("&N?" ?\Ñ)
             ("&O!" ?\Ò)
             ("&O'" ?\Ó)
             ("&O>" ?\Ô)
             ("&O?" ?\Õ)
             ("&O:" ?\Ö)
             ("&*X" ?\×)
             ("&O/" ?\Ø)
             ("&U!" ?\Ù)
             ("&U'" ?\Ú)
             ("&U>" ?\Û)
             ("&U:" ?\Ü)
             ("&Y'" ?\Ý)
             ("&TH" ?\Þ)
             ("&ss" ?\ß)
             ("&a!" ?\à)
             ("&a'" ?\á)
             ("&a>" ?\â)
             ("&a?" ?\ã)
             ("&a:" ?\ä)
             ("&aa" ?\å)
             ("&ae" ?\æ)
             ("&c," ?\ç)
             ("&e!" ?\è)
             ("&e'" ?\é)
             ("&e>" ?\ê)
             ("&e:" ?\ë)
             ("&i!" ?\ì)
             ("&i'" ?\í)
             ("&i>" ?\î)
             ("&i:" ?\ï)
             ("&d-" ?\ð)
             ("&n?" ?\ñ)
             ("&o!" ?\ò)
             ("&o'" ?\ó)
             ("&o>" ?\ô)
             ("&o?" ?\õ)
             ("&o:" ?\ö)
             ("&-:" ?\÷)
             ("&o/" ?\ø)
             ("&u!" ?\ù)
             ("&u'" ?\ú)
             ("&u>" ?\û)
             ("&u:" ?\ü)
             ("&y'" ?\ý)
             ("&th" ?\þ)
             ("&y:" ?\ÿ)
             ("&A-" ?\Ā)
             ("&a-" ?\ā)
             ("&A(" ?\Ă)
             ("&a(" ?\ă)
             ("&A;" ?\Ą)
             ("&a;" ?\ą)
             ("&C'" ?\Ć)
             ("&c'" ?\ć)
             ("&C>" ?\Ĉ)
             ("&c>" ?\ĉ)
             ("&C." ?\Ċ)
             ("&c." ?\ċ)
             ("&C<" ?\Č)
             ("&c<" ?\č)
             ("&D<" ?\Ď)
             ("&d<" ?\ď)
             ("&D/" ?\Đ)
             ("&d/" ?\đ)
             ("&E-" ?\Ē)
             ("&e-" ?\ē)
             ("&E(" ?\Ĕ)
             ("&e(" ?\ĕ)
             ("&E." ?\Ė)
             ("&e." ?\ė)
             ("&E;" ?\Ę)
             ("&e;" ?\ę)
             ("&E<" ?\Ě)
             ("&e<" ?\ě)
             ("&G>" ?\Ĝ)
             ("&g>" ?\ĝ)
             ("&G(" ?\Ğ)
             ("&g(" ?\ğ)
             ("&G." ?\Ġ)
             ("&g." ?\ġ)
             ("&G," ?\Ģ)
             ("&g," ?\ģ)
             ("&H>" ?\Ĥ)
             ("&h>" ?\ĥ)
             ("&H/" ?\Ħ)
             ("&h/" ?\ħ)
             ("&I?" ?\Ĩ)
             ("&i?" ?\ĩ)
             ("&I-" ?\Ī)
             ("&i-" ?\ī)
             ("&I(" ?\Ĭ)
             ("&i(" ?\ĭ)
             ("&I;" ?\Į)
             ("&i;" ?\į)
             ("&I." ?\İ)
             ("&i." ?\ı)
             ("&IJ" ?\Ĳ)
             ("&ij" ?\ĳ)
             ("&J>" ?\Ĵ)
             ("&j>" ?\ĵ)
             ("&K," ?\Ķ)
             ("&k," ?\ķ)
             ("&kk" ?\ĸ)
             ("&L'" ?\Ĺ)
             ("&l'" ?\ĺ)
             ("&L," ?\Ļ)
             ("&l," ?\ļ)
             ("&L<" ?\Ľ)
             ("&l<" ?\ľ)
             ("&L." ?\Ŀ)
             ("&l." ?\ŀ)
             ("&L/" ?\Ł)
             ("&l/" ?\ł)
             ("&N'" ?\Ń)
             ("&n'" ?\ń)
             ("&N," ?\Ņ)
             ("&n," ?\ņ)
             ("&N<" ?\Ň)
             ("&n<" ?\ň)
             ("&'n" ?\ŉ)
             ("&NG" ?\Ŋ)
             ("&ng" ?\ŋ)
             ("&O-" ?\Ō)
             ("&o-" ?\ō)
             ("&O(" ?\Ŏ)
             ("&o(" ?\ŏ)
             ("&O\"" ?\Ő)
             ("&o\"" ?\ő)
             ("&OE" ?\Œ)
             ("&oe" ?\œ)
             ("&R'" ?\Ŕ)
             ("&r'" ?\ŕ)
             ("&R," ?\Ŗ)
             ("&r," ?\ŗ)
             ("&R<" ?\Ř)
             ("&r<" ?\ř)
             ("&S'" ?\Ś)
             ("&s'" ?\ś)
             ("&S>" ?\Ŝ)
             ("&s>" ?\ŝ)
             ("&S," ?\Ş)
             ("&s," ?\ş)
             ("&S<" ?\Š)
             ("&s<" ?\š)
             ("&T," ?\Ţ)
             ("&t," ?\ţ)
             ("&T<" ?\Ť)
             ("&t<" ?\ť)
             ("&T/" ?\Ŧ)
             ("&t/" ?\ŧ)
             ("&U?" ?\Ũ)
             ("&u?" ?\ũ)
             ("&U-" ?\Ū)
             ("&u-" ?\ū)
             ("&U(" ?\Ŭ)
             ("&u(" ?\ŭ)
             ("&U0" ?\Ů)
             ("&u0" ?\ů)
             ("&U\"" ?\Ű)
             ("&u\"" ?\ű)
             ("&U;" ?\Ų)
             ("&u;" ?\ų)
             ("&W>" ?\Ŵ)
             ("&w>" ?\ŵ)
             ("&Y>" ?\Ŷ)
             ("&y>" ?\ŷ)
             ("&Y:" ?\Ÿ)
             ("&Z'" ?\Ź)
             ("&z'" ?\ź)
             ("&Z." ?\Ż)
             ("&z." ?\ż)
             ("&Z<" ?\Ž)
             ("&z<" ?\ž)
             ("&s1" ?\ſ)
             ("&b/" ?\ƀ)
             ("&B2" ?\Ɓ)
             ("&C2" ?\Ƈ)
             ("&c2" ?\ƈ)
             ("&F2" ?\Ƒ)
             ("&f2" ?\ƒ)
             ("&K2" ?\Ƙ)
             ("&k2" ?\ƙ)
             ("&O9" ?\Ơ)
             ("&o9" ?\ơ)
             ("&OI" ?\Ƣ)
             ("&oi" ?\ƣ)
             ("&yr" ?\Ʀ)
             ("&U9" ?\Ư)
             ("&u9" ?\ư)
             ("&Z/" ?\Ƶ)
             ("&z/" ?\ƶ)
             ("&ED" ?\Ʒ)
             ("&DZ<" ?\Ǆ)
             ("&Dz<" ?\ǅ)
             ("&dz<" ?\ǆ)
             ("&LJ3" ?\Ǉ)
             ("&Lj3" ?\ǈ)
             ("&lj3" ?\ǉ)
             ("&NJ3" ?\Ǌ)
             ("&Nj3" ?\ǋ)
             ("&nj3" ?\ǌ)
             ("&A<" ?\Ǎ)
             ("&a<" ?\ǎ)
             ("&I<" ?\Ǐ)
             ("&i<" ?\ǐ)
             ("&O<" ?\Ǒ)
             ("&o<" ?\ǒ)
             ("&U<" ?\Ǔ)
             ("&u<" ?\ǔ)
             ("&U:-" ?\Ǖ)
             ("&u:-" ?\ǖ)
             ("&U:'" ?\Ǘ)
             ("&u:'" ?\ǘ)
             ("&U:<" ?\Ǚ)
             ("&u:<" ?\ǚ)
             ("&U:!" ?\Ǜ)
             ("&u:!" ?\ǜ)
             ("&e1" ?\ǝ)
             ("&A1" ?\Ǟ)
             ("&a1" ?\ǟ)
             ("&A7" ?\Ǡ)
             ("&a7" ?\ǡ)
             ("&A3" ?\Ǣ)
             ("&a3" ?\ǣ)
             ("&G/" ?\Ǥ)
             ("&g/" ?\ǥ)
             ("&G<" ?\Ǧ)
             ("&g<" ?\ǧ)
             ("&K<" ?\Ǩ)
             ("&k<" ?\ǩ)
             ("&O;" ?\Ǫ)
             ("&o;" ?\ǫ)
             ("&O1" ?\Ǭ)
             ("&o1" ?\ǭ)
             ("&EZ" ?\Ǯ)
             ("&ez" ?\ǯ)
             ("&j<" ?\ǰ)
             ("&DZ3" ?\Ǳ)
             ("&Dz3" ?\ǲ)
             ("&dz3" ?\ǳ)
             ("&G'" ?\Ǵ)
             ("&g'" ?\ǵ)
             ("&AA'" ?\Ǻ)
             ("&aa'" ?\ǻ)
             ("&AE'" ?\Ǽ)
             ("&ae'" ?\ǽ)
             ("&O/'" ?\Ǿ)
             ("&o/'" ?\ǿ)
             ("&A!!" ?\Ȁ)
             ("&a!!" ?\ȁ)
             ("&A)" ?\Ȃ)
             ("&a)" ?\ȃ)
             ("&E!!" ?\Ȅ)
             ("&e!!" ?\ȅ)
             ("&E)" ?\Ȇ)
             ("&e)" ?\ȇ)
             ("&I!!" ?\Ȉ)
             ("&i!!" ?\ȉ)
             ("&I)" ?\Ȋ)
             ("&i)" ?\ȋ)
             ("&O!!" ?\Ȍ)
             ("&o!!" ?\ȍ)
             ("&O)" ?\Ȏ)
             ("&o)" ?\ȏ)
             ("&R!!" ?\Ȑ)
             ("&r!!" ?\ȑ)
             ("&R)" ?\Ȓ)
             ("&r)" ?\ȓ)
             ("&U!!" ?\Ȕ)
             ("&u!!" ?\ȕ)
             ("&U)" ?\Ȗ)
             ("&u)" ?\ȗ)
             ("&r1" ?\ɼ)
             ("&ed" ?\ʒ)
             ("&;S" ?\ʻ)
             ("&1>" ?\ˆ)
             ("&'<" ?\ˇ)
             ("&1-" ?\ˉ)
             ("&1!" ?\ˋ)
             ("&'(" ?\˘)
             ("&'." ?\˙)
             ("&'0" ?\˚)
             ("&';" ?\˛)
             ("&1?" ?\˜)
             ("&'\"" ?\˝)
             ("&'G" ?\ʹ)
             ("&,G" ?\͵)
             ("&j3" ?\ͺ)
             ("&?%" ?\;)
             ("&'*" ?\΄)
             ("&'%" ?\΅)
             ("&A%" ?\Ά)
             ("&.*" ?\·)
             ("&E%" ?\Έ)
             ("&Y%" ?\Ή)
             ("&I%" ?\Ί)
             ("&O%" ?\Ό)
             ("&U%" ?\Ύ)
             ("&W%" ?\Ώ)
             ("&i3" ?\ΐ)
             ("&A*" ?\Α)
             ("&B*" ?\Β)
             ("&G*" ?\Γ)
             ("&D*" ?\Δ)
             ("&E*" ?\Ε)
             ("&Z*" ?\Ζ)
             ("&Y*" ?\Η)
             ("&H*" ?\Θ)
             ("&I*" ?\Ι)
             ("&K*" ?\Κ)
             ("&L*" ?\Λ)
             ("&M*" ?\Μ)
             ("&N*" ?\Ν)
             ("&C*" ?\Ξ)
             ("&O*" ?\Ο)
             ("&P*" ?\Π)
             ("&R*" ?\Ρ)
             ("&S*" ?\Σ)
             ("&T*" ?\Τ)
             ("&U*" ?\Υ)
             ("&F*" ?\Φ)
             ("&X*" ?\Χ)
             ("&Q*" ?\Ψ)
             ("&W*" ?\Ω)
             ("&J*" ?\Ϊ)
             ("&V*" ?\Ϋ)
             ("&a%" ?\ά)
             ("&e%" ?\έ)
             ("&y%" ?\ή)
             ("&i%" ?\ί)
             ("&u3" ?\ΰ)
             ("&a*" ?\α)
             ("&b*" ?\β)
             ("&g*" ?\γ)
             ("&d*" ?\δ)
             ("&e*" ?\ε)
             ("&z*" ?\ζ)
             ("&y*" ?\η)
             ("&h*" ?\θ)
             ("&i*" ?\ι)
             ("&k*" ?\κ)
             ("&l*" ?\λ)
             ("&m*" ?\μ)
             ("&n*" ?\ν)
             ("&c*" ?\ξ)
             ("&o*" ?\ο)
             ("&p*" ?\π)
             ("&r*" ?\ρ)
             ("&*s" ?\ς)
             ("&s*" ?\σ)
             ("&t*" ?\τ)
             ("&u*" ?\υ)
             ("&f*" ?\φ)
             ("&x*" ?\χ)
             ("&q*" ?\ψ)
             ("&w*" ?\ω)
             ("&j*" ?\ϊ)
             ("&v*" ?\ϋ)
             ("&o%" ?\ό)
             ("&u%" ?\ύ)
             ("&w%" ?\ώ)
             ("&b3" ?\ϐ)
             ("&T3" ?\Ϛ)
             ("&M3" ?\Ϝ)
             ("&K3" ?\Ϟ)
             ("&P3" ?\Ϡ)
             ("&IO" ?\Ё)
             ("&D%" ?\Ђ)
             ("&G%" ?\Ѓ)
             ("&IE" ?\Є)
             ("&DS" ?\Ѕ)
             ("&II" ?\І)
             ("&YI" ?\Ї)
             ("&J%" ?\Ј)
             ("&LJ" ?\Љ)
             ("&NJ" ?\Њ)
             ("&Ts" ?\Ћ)
             ("&KJ" ?\Ќ)
             ("&V%" ?\Ў)
             ("&DZ" ?\Џ)
             ("&A=" ?\А)
             ("&B=" ?\Б)
             ("&V=" ?\В)
             ("&G=" ?\Г)
             ("&D=" ?\Д)
             ("&E=" ?\Е)
             ("&Z%" ?\Ж)
             ("&Z=" ?\З)
             ("&I=" ?\И)
             ("&J=" ?\Й)
             ("&K=" ?\К)
             ("&L=" ?\Л)
             ("&M=" ?\М)
             ("&N=" ?\Н)
             ("&O=" ?\О)
             ("&P=" ?\П)
             ("&R=" ?\Р)
             ("&S=" ?\С)
             ("&T=" ?\Т)
             ("&U=" ?\У)
             ("&F=" ?\Ф)
             ("&H=" ?\Х)
             ("&C=" ?\Ц)
             ("&C%" ?\Ч)
             ("&S%" ?\Ш)
             ("&Sc" ?\Щ)
             ("&=\"" ?\Ъ)
             ("&Y=" ?\Ы)
             ("&%\"" ?\Ь)
             ("&JE" ?\Э)
             ("&JU" ?\Ю)
             ("&JA" ?\Я)
             ("&a=" ?\а)
             ("&b=" ?\б)
             ("&v=" ?\в)
             ("&g=" ?\г)
             ("&d=" ?\д)
             ("&e=" ?\е)
             ("&z%" ?\ж)
             ("&z=" ?\з)
             ("&i=" ?\и)
             ("&j=" ?\й)
             ("&k=" ?\к)
             ("&l=" ?\л)
             ("&m=" ?\м)
             ("&n=" ?\н)
             ("&o=" ?\о)
             ("&p=" ?\п)
             ("&r=" ?\р)
             ("&s=" ?\с)
             ("&t=" ?\т)
             ("&u=" ?\у)
             ("&f=" ?\ф)
             ("&h=" ?\х)
             ("&c=" ?\ц)
             ("&c%" ?\ч)
             ("&s%" ?\ш)
             ("&sc" ?\щ)
             ("&='" ?\ъ)
             ("&y=" ?\ы)
             ("&%'" ?\ь)
             ("&je" ?\э)
             ("&ju" ?\ю)
             ("&ja" ?\я)
             ("&io" ?\ё)
             ("&d%" ?\ђ)
             ("&g%" ?\ѓ)
             ("&ie" ?\є)
             ("&ds" ?\ѕ)
             ("&ii" ?\і)
             ("&yi" ?\ї)
             ("&j%" ?\ј)
             ("&lj" ?\љ)
             ("&nj" ?\њ)
             ("&ts" ?\ћ)
             ("&kj" ?\ќ)
             ("&v%" ?\ў)
             ("&dz" ?\џ)
             ("&Y3" ?\Ѣ)
             ("&y3" ?\ѣ)
             ("&O3" ?\Ѫ)
             ("&o3" ?\ѫ)
             ("&F3" ?\Ѳ)
             ("&f3" ?\ѳ)
             ("&V3" ?\Ѵ)
             ("&v3" ?\ѵ)
             ("&C3" ?\Ҁ)
             ("&c3" ?\ҁ)
             ("&G3" ?\Ґ)
             ("&g3" ?\ґ)
             ("&A+" ?\א)
             ("&B+" ?\ב)
             ("&G+" ?\ג)
             ("&D+" ?\ד)
             ("&H+" ?\ה)
             ("&W+" ?\ו)
             ("&Z+" ?\ז)
             ("&X+" ?\ח)
             ("&Tj" ?\ט)
             ("&J+" ?\י)
             ("&K%" ?\ך)
             ("&K+" ?\כ)
             ("&L+" ?\ל)
             ("&M%" ?\ם)
             ("&M+" ?\מ)
             ("&N%" ?\ן)
             ("&N+" ?\נ)
             ("&S+" ?\ס)
             ("&E+" ?\ע)
             ("&P%" ?\ף)
             ("&P+" ?\פ)
             ("&Zj" ?\ץ)
             ("&ZJ" ?\צ)
             ("&Q+" ?\ק)
             ("&R+" ?\ר)
             ("&Sh" ?\ש)
             ("&T+" ?\ת)
             ("&,+" ?\،)
             ("&;+" ?\؛)
             ("&?+" ?\؟)
             ("&H'" ?\ء)
             ("&aM" ?\آ)
             ("&aH" ?\أ)
             ("&wH" ?\ؤ)
             ("&ah" ?\إ)
             ("&yH" ?\ئ)
             ("&a+" ?\ا)
             ("&b+" ?\ب)
             ("&tm" ?\ة)
             ("&t+" ?\ت)
             ("&tk" ?\ث)
             ("&g+" ?\ج)
             ("&hk" ?\ح)
             ("&x+" ?\خ)
             ("&d+" ?\د)
             ("&dk" ?\ذ)
             ("&r+" ?\ر)
             ("&z+" ?\ز)
             ("&s+" ?\س)
             ("&sn" ?\ش)
             ("&c+" ?\ص)
             ("&dd" ?\ض)
             ("&tj" ?\ط)
             ("&zH" ?\ظ)
             ("&e+" ?\ع)
             ("&i+" ?\غ)
             ("&++" ?\ـ)
             ("&f+" ?\ف)
             ("&q+" ?\ق)
             ("&k+" ?\ك)
             ("&l+" ?\ل)
             ("&m+" ?\م)
             ("&n+" ?\ن)
             ("&h+" ?\ه)
             ("&w+" ?\و)
             ("&j+" ?\ى)
             ("&y+" ?\ي)
             ("&:+" ?\ً)
             ("&\"+" ?\ٌ)
             ("&=+" ?\ٍ)
             ("&/+" ?\َ)
             ("&'+" ?\ُ)
             ("&1+" ?\ِ)
             ("&3+" ?\ّ)
             ("&0+" ?\ْ)
             ("&0a" ?\٠)
             ("&1a" ?\١)
             ("&2a" ?\٢)
             ("&3a" ?\٣)
             ("&4a" ?\٤)
             ("&5a" ?\٥)
             ("&6a" ?\٦)
             ("&7a" ?\٧)
             ("&8a" ?\٨)
             ("&9a" ?\٩)
             ("&aS" ?\ٰ)
             ("&p+" ?\پ)
             ("&hH" ?\ځ)
             ("&tc" ?\چ)
             ("&zj" ?\ژ)
             ("&v+" ?\ڤ)
             ("&gf" ?\گ)
             ("&A-0" ?\Ḁ)
             ("&a-0" ?\ḁ)
             ("&B." ?\Ḃ)
             ("&b." ?\ḃ)
             ("&B-." ?\Ḅ)
             ("&b-." ?\ḅ)
             ("&B_" ?\Ḇ)
             ("&b_" ?\ḇ)
             ("&C,'" ?\Ḉ)
             ("&c,'" ?\ḉ)
             ("&D." ?\Ḋ)
             ("&d." ?\ḋ)
             ("&D-." ?\Ḍ)
             ("&d-." ?\ḍ)
             ("&D_" ?\Ḏ)
             ("&d_" ?\ḏ)
             ("&D," ?\Ḑ)
             ("&d," ?\ḑ)
             ("&D->" ?\Ḓ)
             ("&d->" ?\ḓ)
             ("&E-!" ?\Ḕ)
             ("&e-!" ?\ḕ)
             ("&E-'" ?\Ḗ)
             ("&e-'" ?\ḗ)
             ("&E->" ?\Ḙ)
             ("&e->" ?\ḙ)
             ("&E-?" ?\Ḛ)
             ("&e-?" ?\ḛ)
             ("&E,(" ?\Ḝ)
             ("&e,(" ?\ḝ)
             ("&F." ?\Ḟ)
             ("&f." ?\ḟ)
             ("&G-" ?\Ḡ)
             ("&g-" ?\ḡ)
             ("&H." ?\Ḣ)
             ("&h." ?\ḣ)
             ("&H-." ?\Ḥ)
             ("&h-." ?\ḥ)
             ("&H:" ?\Ḧ)
             ("&h:" ?\ḧ)
             ("&H," ?\Ḩ)
             ("&h," ?\ḩ)
             ("&H-(" ?\Ḫ)
             ("&h-(" ?\ḫ)
             ("&I-?" ?\Ḭ)
             ("&i-?" ?\ḭ)
             ("&I:'" ?\Ḯ)
             ("&i:'" ?\ḯ)
             ("&K'" ?\Ḱ)
             ("&k'" ?\ḱ)
             ("&K-." ?\Ḳ)
             ("&k-." ?\ḳ)
             ("&K_" ?\Ḵ)
             ("&k_" ?\ḵ)
             ("&L-." ?\Ḷ)
             ("&l-." ?\ḷ)
             ("&L_" ?\Ḻ)
             ("&l_" ?\ḻ)
             ("&L->" ?\Ḽ)
             ("&l->" ?\ḽ)
             ("&M'" ?\Ḿ)
             ("&m'" ?\ḿ)
             ("&M." ?\Ṁ)
             ("&m." ?\ṁ)
             ("&M-." ?\Ṃ)
             ("&m-." ?\ṃ)
             ("&N." ?\Ṅ)
             ("&n." ?\ṅ)
             ("&N-." ?\Ṇ)
             ("&n-." ?\ṇ)
             ("&N_" ?\Ṉ)
             ("&n_" ?\ṉ)
             ("&N->" ?\Ṋ)
             ("&n->" ?\ṋ)
             ("&O?'" ?\Ṍ)
             ("&o?'" ?\ṍ)
             ("&O?:" ?\Ṏ)
             ("&o?:" ?\ṏ)
             ("&O-!" ?\Ṑ)
             ("&o-!" ?\ṑ)
             ("&O-'" ?\Ṓ)
             ("&o-'" ?\ṓ)
             ("&P'" ?\Ṕ)
             ("&p'" ?\ṕ)
             ("&P." ?\Ṗ)
             ("&p." ?\ṗ)
             ("&R." ?\Ṙ)
             ("&r." ?\ṙ)
             ("&R-." ?\Ṛ)
             ("&r-." ?\ṛ)
             ("&R_" ?\Ṟ)
             ("&r_" ?\ṟ)
             ("&S." ?\Ṡ)
             ("&s." ?\ṡ)
             ("&S-." ?\Ṣ)
             ("&s-." ?\ṣ)
             ("&S'." ?\Ṥ)
             ("&s'." ?\ṥ)
             ("&S<." ?\Ṧ)
             ("&s<." ?\ṧ)
             ("&T." ?\Ṫ)
             ("&t." ?\ṫ)
             ("&T-." ?\Ṭ)
             ("&t-." ?\ṭ)
             ("&T_" ?\Ṯ)
             ("&t_" ?\ṯ)
             ("&T->" ?\Ṱ)
             ("&t->" ?\ṱ)
             ("&U-?" ?\Ṵ)
             ("&u-?" ?\ṵ)
             ("&U->" ?\Ṷ)
             ("&u->" ?\ṷ)
             ("&U?'" ?\Ṹ)
             ("&u?'" ?\ṹ)
             ("&U-:" ?\Ṻ)
             ("&u-:" ?\ṻ)
             ("&V?" ?\Ṽ)
             ("&v?" ?\ṽ)
             ("&V-." ?\Ṿ)
             ("&v-." ?\ṿ)
             ("&W!" ?\Ẁ)
             ("&w!" ?\ẁ)
             ("&W'" ?\Ẃ)
             ("&w'" ?\ẃ)
             ("&W:" ?\Ẅ)
             ("&w:" ?\ẅ)
             ("&W." ?\Ẇ)
             ("&w." ?\ẇ)
             ("&W-." ?\Ẉ)
             ("&w-." ?\ẉ)
             ("&X." ?\Ẋ)
             ("&x." ?\ẋ)
             ("&X:" ?\Ẍ)
             ("&x:" ?\ẍ)
             ("&Y." ?\Ẏ)
             ("&y." ?\ẏ)
             ("&Z>" ?\Ẑ)
             ("&z>" ?\ẑ)
             ("&Z-." ?\Ẓ)
             ("&z-." ?\ẓ)
             ("&Z_" ?\Ẕ)
             ("&z_" ?\ẕ)
             ("&A-." ?\Ạ)
             ("&a-." ?\ạ)
             ("&A2" ?\Ả)
             ("&a2" ?\ả)
             ("&A>'" ?\Ấ)
             ("&a>'" ?\ấ)
             ("&A>!" ?\Ầ)
             ("&a>!" ?\ầ)
             ("&A>2" ?\Ẩ)
             ("&a>2" ?\ẩ)
             ("&A>?" ?\Ẫ)
             ("&a>?" ?\ẫ)
             ("&A('" ?\Ắ)
             ("&a('" ?\ắ)
             ("&A(!" ?\Ằ)
             ("&a(!" ?\ằ)
             ("&A(2" ?\Ẳ)
             ("&a(2" ?\ẳ)
             ("&A(?" ?\Ẵ)
             ("&a(?" ?\ẵ)
             ("&E-." ?\Ẹ)
             ("&e-." ?\ẹ)
             ("&E2" ?\Ẻ)
             ("&e2" ?\ẻ)
             ("&E?" ?\Ẽ)
             ("&e?" ?\ẽ)
             ("&E>'" ?\Ế)
             ("&e>'" ?\ế)
             ("&E>!" ?\Ề)
             ("&e>!" ?\ề)
             ("&E>2" ?\Ể)
             ("&e>2" ?\ể)
             ("&E>?" ?\Ễ)
             ("&e>?" ?\ễ)
             ("&I2" ?\Ỉ)
             ("&i2" ?\ỉ)
             ("&I-." ?\Ị)
             ("&i-." ?\ị)
             ("&O-." ?\Ọ)
             ("&o-." ?\ọ)
             ("&O2" ?\Ỏ)
             ("&o2" ?\ỏ)
             ("&O>'" ?\Ố)
             ("&o>'" ?\ố)
             ("&O>!" ?\Ồ)
             ("&o>!" ?\ồ)
             ("&O>2" ?\Ổ)
             ("&o>2" ?\ổ)
             ("&O>?" ?\Ỗ)
             ("&o>?" ?\ỗ)
             ("&O9'" ?\Ớ)
             ("&o9'" ?\ớ)
             ("&O9!" ?\Ờ)
             ("&o9!" ?\ờ)
             ("&O92" ?\Ở)
             ("&o92" ?\ở)
             ("&O9?" ?\Ỡ)
             ("&o9?" ?\ỡ)
             ("&U-." ?\Ụ)
             ("&u-." ?\ụ)
             ("&U2" ?\Ủ)
             ("&u2" ?\ủ)
             ("&U9'" ?\Ứ)
             ("&u9'" ?\ứ)
             ("&U9!" ?\Ừ)
             ("&u9!" ?\ừ)
             ("&U92" ?\Ử)
             ("&u92" ?\ử)
             ("&U9?" ?\Ữ)
             ("&u9?" ?\ữ)
             ("&Y!" ?\Ỳ)
             ("&y!" ?\ỳ)
             ("&Y-." ?\Ỵ)
             ("&y-." ?\ỵ)
             ("&Y2" ?\Ỷ)
             ("&y2" ?\ỷ)
             ("&Y?" ?\Ỹ)
             ("&y?" ?\ỹ)
             ("&a*," ?\ἀ)
             ("&a*;" ?\ἁ)
             ("&A*," ?\Ἀ)
             ("&A*;" ?\Ἁ)
             ("&e*," ?\ἐ)
             ("&e*;" ?\ἑ)
             ("&E*," ?\Ἐ)
             ("&E*;" ?\Ἑ)
             ("&y*," ?\ἠ)
             ("&y*;" ?\ἡ)
             ("&Y*," ?\Ἠ)
             ("&Y*;" ?\Ἡ)
             ("&i*," ?\ἰ)
             ("&i*;" ?\ἱ)
             ("&I*," ?\Ἰ)
             ("&I*;" ?\Ἱ)
             ("&o*," ?\ὀ)
             ("&o*;" ?\ὁ)
             ("&O*," ?\Ὀ)
             ("&O*;" ?\Ὁ)
             ("&u*," ?\ὐ)
             ("&u*;" ?\ὑ)
             ("&U*;" ?\Ὑ)
             ("&w*," ?\ὠ)
             ("&w*;" ?\ὡ)
             ("&W*," ?\Ὠ)
             ("&W*;" ?\Ὡ)
             ("&a*!" ?\ὰ)
             ("&a*'" ?\ά)
             ("&e*!" ?\ὲ)
             ("&e*'" ?\έ)
             ("&y*!" ?\ὴ)
             ("&y*'" ?\ή)
             ("&i*!" ?\ὶ)
             ("&i*'" ?\ί)
             ("&o*!" ?\ὸ)
             ("&o*'" ?\ό)
             ("&u*!" ?\ὺ)
             ("&u*'" ?\ύ)
             ("&w*!" ?\ὼ)
             ("&w*'" ?\ώ)
             ("&a*(" ?\ᾰ)
             ("&a*-" ?\ᾱ)
             ("&a*j" ?\ᾳ)
             ("&a*?" ?\ᾶ)
             ("&A*(" ?\Ᾰ)
             ("&A*-" ?\Ᾱ)
             ("&A*!" ?\Ὰ)
             ("&A*'" ?\Ά)
             ("&A*J" ?\ᾼ)
             ("&)*" ?\᾽)
             ("&J3" ?\ι)
             ("&,," ?\᾿)
             ("&?*" ?\῀)
             ("&?:" ?\῁)
             ("&y*j" ?\ῃ)
             ("&y*?" ?\ῆ)
             ("&E*'" ?\Έ)
             ("&Y*!" ?\Ὴ)
             ("&Y*'" ?\Ή)
             ("&Y*J" ?\ῌ)
             ("&,!" ?\῍)
             ("&,'" ?\῎)
             ("&?," ?\῏)
             ("&i*(" ?\ῐ)
             ("&i*-" ?\ῑ)
             ("&i*?" ?\ῖ)
             ("&I*(" ?\Ῐ)
             ("&I*-" ?\Ῑ)
             ("&I*!" ?\Ὶ)
             ("&I*'" ?\Ί)
             ("&;!" ?\῝)
             ("&;'" ?\῞)
             ("&?;" ?\῟)
             ("&u*(" ?\ῠ)
             ("&u*-" ?\ῡ)
             ("&r*," ?\ῤ)
             ("&r*;" ?\ῥ)
             ("&u*?" ?\ῦ)
             ("&U*(" ?\Ῠ)
             ("&U*-" ?\Ῡ)
             ("&U*!" ?\Ὺ)
             ("&U*'" ?\Ύ)
             ("&R*;" ?\Ῥ)
             ("&!:" ?\῭)
             ("&:'" ?\΅)
             ("&!*" ?\`)
             ("&w*j" ?\ῳ)
             ("&w*?" ?\ῶ)
             ("&O*!" ?\Ὸ)
             ("&O*'" ?\Ό)
             ("&W*!" ?\Ὼ)
             ("&W*'" ?\Ώ)
             ("&W*J" ?\ῼ)
             ("&/*" ?\´)
             ("&;;" ?\῾)
             ("&1N" ?\ )
             ("&1M" ?\ )
             ("&3M" ?\ )
             ("&4M" ?\ )
             ("&6M" ?\ )
             ("&1T" ?\ )
             ("&1H" ?\ )
             ("&LR" ?\‎)
             ("&RL" ?\‏)
             ("&-1" ?\‐)
             ("&-N" ?\–)
             ("&-M" ?\—)
             ("&-3" ?\―)
             ("&!2" ?\‖)
             ("&=2" ?\‗)
             ("&'6" ?\‘)
             ("&'9" ?\’)
             ("&.9" ?\‚)
             ("&9'" ?\‛)
             ("&\"6" ?\“)
             ("&\"9" ?\”)
             ("&:9" ?\„)
             ("&9\"" ?\‟)
             ("&/-" ?\†)
             ("&/=" ?\‡)
             ("&sb" ?\•)
             ("&3b" ?\‣)
             ("&.." ?\‥)
             ("&.3" ?\…)
             ("&.-" ?\‧)
             ("&%0" ?\‰)
             ("&1'" ?\′)
             ("&2'" ?\″)
             ("&3'" ?\‴)
             ("&1\"" ?\‵)
             ("&2\"" ?\‶)
             ("&3\"" ?\‷)
             ("&Ca" ?\‸)
             ("&<1" ?\‹)
             ("&>1" ?\›)
             ("&:X" ?\※)
             ("&!*2" ?\‼)
             ("&'-" ?\‾)
             ("&-b" ?\⁃)
             ("&/f" ?\⁄)
             ("&0S" ?\⁰)
             ("&4S" ?\⁴)
             ("&5S" ?\⁵)
             ("&6S" ?\⁶)
             ("&7S" ?\⁷)
             ("&8S" ?\⁸)
             ("&9S" ?\⁹)
             ("&+S" ?\⁺)
             ("&-S" ?\⁻)
             ("&=S" ?\⁼)
             ("&(S" ?\⁽)
             ("&)S" ?\⁾)
             ("&nS" ?\ⁿ)
             ("&0s" ?\₀)
             ("&1s" ?\₁)
             ("&2s" ?\₂)
             ("&3s" ?\₃)
             ("&4s" ?\₄)
             ("&5s" ?\₅)
             ("&6s" ?\₆)
             ("&7s" ?\₇)
             ("&8s" ?\₈)
             ("&9s" ?\₉)
             ("&+s" ?\₊)
             ("&-s" ?\₋)
             ("&=s" ?\₌)
             ("&(s" ?\₍)
             ("&)s" ?\₎)
             ("&Ff" ?\₣)
             ("&Li" ?\₤)
             ("&Pt" ?\₧)
             ("&W=" ?\₩)
             ("&NSh" ?\₪)
             ("&Eu" ?\€)
             ("&\"7" ?\⃑)
             ("&oC" ?\℃)
             ("&co" ?\℅)
             ("&oF" ?\℉)
             ("&N0" ?\№)
             ("&PO" ?\℗)
             ("&Rx" ?\℞)
             ("&SM" ?\℠)
             ("&TM" ?\™)
             ("&Om" ?\Ω)
             ("&AO" ?\Å)
             ("&Est" ?\℮)
             ("&13" ?\⅓)
             ("&23" ?\⅔)
             ("&15" ?\⅕)
             ("&25" ?\⅖)
             ("&35" ?\⅗)
             ("&45" ?\⅘)
             ("&16" ?\⅙)
             ("&56" ?\⅚)
             ("&18" ?\⅛)
             ("&38" ?\⅜)
             ("&58" ?\⅝)
             ("&78" ?\⅞)
             ("&1R" ?\Ⅰ)
             ("&2R" ?\Ⅱ)
             ("&3R" ?\Ⅲ)
             ("&4R" ?\Ⅳ)
             ("&5R" ?\Ⅴ)
             ("&6R" ?\Ⅵ)
             ("&7R" ?\Ⅶ)
             ("&8R" ?\Ⅷ)
             ("&9R" ?\Ⅸ)
             ("&aR" ?\Ⅹ)
             ("&bR" ?\Ⅺ)
             ("&cR" ?\Ⅻ)
             ("&50R" ?\Ⅼ)
             ("&1r" ?\ⅰ)
             ("&2r" ?\ⅱ)
             ("&3r" ?\ⅲ)
             ("&4r" ?\ⅳ)
             ("&5r" ?\ⅴ)
             ("&6r" ?\ⅵ)
             ("&7r" ?\ⅶ)
             ("&8r" ?\ⅷ)
             ("&9r" ?\ⅸ)
             ("&ar" ?\ⅹ)
             ("&br" ?\ⅺ)
             ("&cr" ?\ⅻ)
             ("&50r" ?\ⅼ)
             ("&<-" ?\←)
             ("&-!" ?\↑)
             ("&->" ?\→)
             ("&-v" ?\↓)
             ("&<>" ?\↔)
             ("&UD" ?\↕)
             ("&<!!" ?\↖)
             ("&//>" ?\↗)
             ("&!!>" ?\↘)
             ("&<//" ?\↙)
             ("&UD-" ?\↨)
             ("&>V" ?\⇀)
             ("&<=" ?\⇐)
             ("&=>" ?\⇒)
             ("&==" ?\⇔)
             ("&FA" ?\∀)
             ("&dP" ?\∂)
             ("&TE" ?\∃)
             ("&/0" ?\∅)
             ("&DE" ?\∆)
             ("&NB" ?\∇)
             ("&(-" ?\∈)
             ("&-)" ?\∋)
             ("&FP" ?\∎)
             ("&*P" ?\∏)
             ("&+Z" ?\∑)
             ("&-2" ?\−)
             ("&-+" ?\∓)
             ("&.+" ?\∔)
             ("&*-" ?\∗)
             ("&Ob" ?\∘)
             ("&Sb" ?\∙)
             ("&RT" ?\√)
             ("&0(" ?\∝)
             ("&00" ?\∞)
             ("&-L" ?\∟)
             ("&-V" ?\∠)
             ("&PP" ?\∥)
             ("&AN" ?\∧)
             ("&OR" ?\∨)
             ("&(U" ?\∩)
             ("&)U" ?\∪)
             ("&In" ?\∫)
             ("&DI" ?\∬)
             ("&Io" ?\∮)
             ("&.:" ?\∴)
             ("&:." ?\∵)
             ("&:R" ?\∶)
             ("&::" ?\∷)
             ("&?1" ?\∼)
             ("&CG" ?\∾)
             ("&?-" ?\≃)
             ("&?=" ?\≅)
             ("&?2" ?\≈)
             ("&=?" ?\≌)
             ("&HI" ?\≓)
             ("&!=" ?\≠)
             ("&=3" ?\≡)
             ("&=<" ?\≤)
             ("&>=" ?\≥)
             ("&<*" ?\≪)
             ("&*>" ?\≫)
             ("&!<" ?\≮)
             ("&!>" ?\≯)
             ("&(C" ?\⊂)
             ("&)C" ?\⊃)
             ("&(_" ?\⊆)
             ("&)_" ?\⊇)
             ("&0." ?\⊙)
             ("&02" ?\⊚)
             ("&-T" ?\⊥)
             ("&.P" ?\⋅)
             ("&:3" ?\⋮)
             ("&Eh" ?\⌂)
             ("&<7" ?\⌈)
             ("&>7" ?\⌉)
             ("&7<" ?\⌊)
             ("&7>" ?\⌋)
             ("&NI" ?\⌐)
             ("&(A" ?\⌒)
             ("&TR" ?\⌕)
             ("&88" ?\⌘)
             ("&Iu" ?\⌠)
             ("&Il" ?\⌡)
             ("&</" ?\〈)
             ("&/>" ?\〉)
             ("&Vs" ?\␣)
             ("&1h" ?\⑀)
             ("&3h" ?\⑁)
             ("&2h" ?\⑂)
             ("&4h" ?\⑃)
             ("&1j" ?\⑆)
             ("&2j" ?\⑇)
             ("&3j" ?\⑈)
             ("&4j" ?\⑉)
             ("&1-o" ?\①)
             ("&2-o" ?\②)
             ("&3-o" ?\③)
             ("&4-o" ?\④)
             ("&5-o" ?\⑤)
             ("&6-o" ?\⑥)
             ("&7-o" ?\⑦)
             ("&8-o" ?\⑧)
             ("&9-o" ?\⑨)
             ("&(1)" ?\⑴)
             ("&(2)" ?\⑵)
             ("&(3)" ?\⑶)
             ("&(4)" ?\⑷)
             ("&(5)" ?\⑸)
             ("&(6)" ?\⑹)
             ("&(7)" ?\⑺)
             ("&(8)" ?\⑻)
             ("&(9)" ?\⑼)
             ("&1." ?\⒈)
             ("&2." ?\⒉)
             ("&3." ?\⒊)
             ("&4." ?\⒋)
             ("&5." ?\⒌)
             ("&6." ?\⒍)
             ("&7." ?\⒎)
             ("&8." ?\⒏)
             ("&9." ?\⒐)
             ("&10." ?\⒑)
             ("&11." ?\⒒)
             ("&12." ?\⒓)
             ("&13." ?\⒔)
             ("&14." ?\⒕)
             ("&15." ?\⒖)
             ("&16." ?\⒗)
             ("&17." ?\⒘)
             ("&18." ?\⒙)
             ("&19." ?\⒚)
             ("&20." ?\⒛)
             ("&(a)" ?\⒜)
             ("&(b)" ?\⒝)
             ("&(c)" ?\⒞)
             ("&(d)" ?\⒟)
             ("&(e)" ?\⒠)
             ("&(f)" ?\⒡)
             ("&(g)" ?\⒢)
             ("&(h)" ?\⒣)
             ("&(i)" ?\⒤)
             ("&(j)" ?\⒥)
             ("&(k)" ?\⒦)
             ("&(l)" ?\⒧)
             ("&(m)" ?\⒨)
             ("&(n)" ?\⒩)
             ("&(o)" ?\⒪)
             ("&(p)" ?\⒫)
             ("&(q)" ?\⒬)
             ("&(r)" ?\⒭)
             ("&(s)" ?\⒮)
             ("&(t)" ?\⒯)
             ("&(u)" ?\⒰)
             ("&(v)" ?\⒱)
             ("&(w)" ?\⒲)
             ("&(x)" ?\⒳)
             ("&(y)" ?\⒴)
             ("&(z)" ?\⒵)
             ("&A-o" ?\Ⓐ)
             ("&B-o" ?\Ⓑ)
             ("&C-o" ?\Ⓒ)
             ("&D-o" ?\Ⓓ)
             ("&E-o" ?\Ⓔ)
             ("&F-o" ?\Ⓕ)
             ("&G-o" ?\Ⓖ)
             ("&H-o" ?\Ⓗ)
             ("&I-o" ?\Ⓘ)
             ("&J-o" ?\Ⓙ)
             ("&K-o" ?\Ⓚ)
             ("&L-o" ?\Ⓛ)
             ("&M-o" ?\Ⓜ)
             ("&N-o" ?\Ⓝ)
             ("&O-o" ?\Ⓞ)
             ("&P-o" ?\Ⓟ)
             ("&Q-o" ?\Ⓠ)
             ("&R-o" ?\Ⓡ)
             ("&S-o" ?\Ⓢ)
             ("&T-o" ?\Ⓣ)
             ("&U-o" ?\Ⓤ)
             ("&V-o" ?\Ⓥ)
             ("&W-o" ?\Ⓦ)
             ("&X-o" ?\Ⓧ)
             ("&Y-o" ?\Ⓨ)
             ("&Z-o" ?\Ⓩ)
             ("&a-o" ?\ⓐ)
             ("&b-o" ?\ⓑ)
             ("&c-o" ?\ⓒ)
             ("&d-o" ?\ⓓ)
             ("&e-o" ?\ⓔ)
             ("&f-o" ?\ⓕ)
             ("&g-o" ?\ⓖ)
             ("&h-o" ?\ⓗ)
             ("&i-o" ?\ⓘ)
             ("&j-o" ?\ⓙ)
             ("&k-o" ?\ⓚ)
             ("&l-o" ?\ⓛ)
             ("&m-o" ?\ⓜ)
             ("&n-o" ?\ⓝ)
             ("&o-o" ?\ⓞ)
             ("&p-o" ?\ⓟ)
             ("&q-o" ?\ⓠ)
             ("&r-o" ?\ⓡ)
             ("&s-o" ?\ⓢ)
             ("&t-o" ?\ⓣ)
             ("&u-o" ?\ⓤ)
             ("&v-o" ?\ⓥ)
             ("&w-o" ?\ⓦ)
             ("&x-o" ?\ⓧ)
             ("&y-o" ?\ⓨ)
             ("&z-o" ?\ⓩ)
             ("&0-o" ?\⓪)
             ("&hh" ?\─)
             ("&HH-" ?\━)
             ("&vv" ?\│)
             ("&VV-" ?\┃)
             ("&3-" ?\┄)
             ("&3_" ?\┅)
             ("&3!" ?\┆)
             ("&3/" ?\┇)
             ("&4-" ?\┈)
             ("&4_" ?\┉)
             ("&4!" ?\┊)
             ("&4/" ?\┋)
             ("&dr" ?\┌)
             ("&dR-" ?\┍)
             ("&Dr-" ?\┎)
             ("&DR-" ?\┏)
             ("&dl" ?\┐)
             ("&dL-" ?\┑)
             ("&Dl-" ?\┒)
             ("&LD-" ?\┓)
             ("&ur" ?\└)
             ("&uR-" ?\┕)
             ("&Ur-" ?\┖)
             ("&UR-" ?\┗)
             ("&ul" ?\┘)
             ("&uL-" ?\┙)
             ("&Ul-" ?\┚)
             ("&UL-" ?\┛)
             ("&vr" ?\├)
             ("&vR-" ?\┝)
             ("&Udr" ?\┞)
             ("&uDr" ?\┟)
             ("&Vr-" ?\┠)
             ("&UdR" ?\┡)
             ("&uDR" ?\┢)
             ("&VR-" ?\┣)
             ("&vl" ?\┤)
             ("&vL-" ?\┥)
             ("&Udl" ?\┦)
             ("&uDl" ?\┧)
             ("&Vl-" ?\┨)
             ("&UdL" ?\┩)
             ("&uDL" ?\┪)
             ("&VL-" ?\┫)
             ("&dh" ?\┬)
             ("&dLr" ?\┭)
             ("&dlR" ?\┮)
             ("&dH-" ?\┯)
             ("&Dh-" ?\┰)
             ("&DLr" ?\┱)
             ("&DlR" ?\┲)
             ("&DH-" ?\┳)
             ("&uh" ?\┴)
             ("&uLr" ?\┵)
             ("&ulR" ?\┶)
             ("&uH-" ?\┷)
             ("&Uh-" ?\┸)
             ("&ULr" ?\┹)
             ("&UlR" ?\┺)
             ("&UH-" ?\┻)
             ("&vh" ?\┼)
             ("&vLr" ?\┽)
             ("&vlR" ?\┾)
             ("&vH-" ?\┿)
             ("&Udh" ?\╀)
             ("&uDh" ?\╁)
             ("&Vh-" ?\╂)
             ("&UdH" ?\╇)
             ("&uDH" ?\╈)
             ("&VLr" ?\╉)
             ("&VlR" ?\╊)
             ("&VH-" ?\╋)
             ("&HH" ?\═)
             ("&VV" ?\║)
             ("&dR" ?\╒)
             ("&Dr" ?\╓)
             ("&DR" ?\╔)
             ("&dL" ?\╕)
             ("&Dl" ?\╖)
             ("&LD" ?\╗)
             ("&uR" ?\╘)
             ("&Ur" ?\╙)
             ("&UR" ?\╚)
             ("&uL" ?\╛)
             ("&Ul" ?\╜)
             ("&UL" ?\╝)
             ("&vR" ?\╞)
             ("&Vr" ?\╟)
             ("&VR" ?\╠)
             ("&vL" ?\╡)
             ("&Vl" ?\╢)
             ("&VL" ?\╣)
             ("&dH" ?\╤)
             ("&Dh" ?\╥)
             ("&DH" ?\╦)
             ("&uH" ?\╧)
             ("&Uh" ?\╨)
             ("&UH" ?\╩)
             ("&vH" ?\╪)
             ("&Vh" ?\╫)
             ("&VH" ?\╬)
             ("&FD" ?\╱)
             ("&BD" ?\╲)
             ("&TB" ?\▀)
             ("&LB" ?\▄)
             ("&FB" ?\█)
             ("&lB" ?\▌)
             ("&RB" ?\▐)
             ("&.S" ?\░)
             ("&:S" ?\▒)
             ("&?S" ?\▓)
             ("&fS" ?\■)
             ("&OS" ?\□)
             ("&RO" ?\▢)
             ("&Rr" ?\▣)
             ("&RF" ?\▤)
             ("&RY" ?\▥)
             ("&RH" ?\▦)
             ("&RZ" ?\▧)
             ("&RK" ?\▨)
             ("&RX" ?\▩)
             ("&sB" ?\▪)
             ("&SR" ?\▬)
             ("&Or" ?\▭)
             ("&UT" ?\▲)
             ("&uT" ?\△)
             ("&Tr" ?\▷)
             ("&PR" ?\►)
             ("&Dt" ?\▼)
             ("&dT" ?\▽)
             ("&Tl" ?\◁)
             ("&PL" ?\◄)
             ("&Db" ?\◆)
             ("&Dw" ?\◇)
             ("&LZ" ?\◊)
             ("&0m" ?\○)
             ("&0o" ?\◎)
             ("&0M" ?\●)
             ("&0L" ?\◐)
             ("&0R" ?\◑)
             ("&Sn" ?\◘)
             ("&Ic" ?\◙)
             ("&Fd" ?\◢)
             ("&Bd" ?\◣)
             ("&Ci" ?\◯)
             ("&*2" ?\★)
             ("&*1" ?\☆)
             ("&TEL" ?\☎)
             ("&tel" ?\☏)
             ("&<H" ?\☜)
             ("&>H" ?\☞)
             ("&0u" ?\☺)
             ("&0U" ?\☻)
             ("&SU" ?\☼)
             ("&Fm" ?\♀)
             ("&Ml" ?\♂)
             ("&cS" ?\♠)
             ("&cH" ?\♡)
             ("&cD" ?\♢)
             ("&cC" ?\♣)
             ("&cS-" ?\♤)
             ("&cH-" ?\♥)
             ("&cD-" ?\♦)
             ("&cC-" ?\♧)
             ("&Md" ?\♩)
             ("&M8" ?\♪)
             ("&M2" ?\♫)
             ("&M16" ?\♬)
             ("&Mb" ?\♭)
             ("&Mx" ?\♮)
             ("&MX" ?\♯)
             ("&OK" ?\✓)
             ("&XX" ?\✗)
             ("&-X" ?\✠)
             ("&IS" ?\　)
             ("&,_" ?\、)
             ("&._" ?\。)
             ("&+\"" ?\〃)
             ("&JIS" ?\〄)
             ("&*_" ?\々)
             ("&;_" ?\〆)
             ("&0_" ?\〇)
             ("&<+" ?\《)
             ("&>+" ?\》)
             ("&<'" ?\「)
             ("&>'" ?\」)
             ("&<\"" ?\『)
             ("&>\"" ?\』)
             ("&(\"" ?\【)
             ("&)\"" ?\】)
             ("&=T" ?\〒)
             ("&=_" ?\〓)
             ("&('" ?\〔)
             ("&)'" ?\〕)
             ("&(I" ?\〖)
             ("&)I" ?\〗)
             ("&-?" ?\〜)
             ("&A5" ?\ぁ)
             ("&a5" ?\あ)
             ("&I5" ?\ぃ)
             ("&i5" ?\い)
             ("&U5" ?\ぅ)
             ("&u5" ?\う)
             ("&E5" ?\ぇ)
             ("&e5" ?\え)
             ("&O5" ?\ぉ)
             ("&o5" ?\お)
             ("&ka" ?\か)
             ("&ga" ?\が)
             ("&ki" ?\き)
             ("&gi" ?\ぎ)
             ("&ku" ?\く)
             ("&gu" ?\ぐ)
             ("&ke" ?\け)
             ("&ge" ?\げ)
             ("&ko" ?\こ)
             ("&go" ?\ご)
             ("&sa" ?\さ)
             ("&za" ?\ざ)
             ("&si" ?\し)
             ("&zi" ?\じ)
             ("&su" ?\す)
             ("&zu" ?\ず)
             ("&se" ?\せ)
             ("&ze" ?\ぜ)
             ("&so" ?\そ)
             ("&zo" ?\ぞ)
             ("&ta" ?\た)
             ("&da" ?\だ)
             ("&ti" ?\ち)
             ("&di" ?\ぢ)
             ("&tU" ?\っ)
             ("&tu" ?\つ)
             ("&du" ?\づ)
             ("&te" ?\て)
             ("&de" ?\で)
             ("&to" ?\と)
             ("&do" ?\ど)
             ("&na" ?\な)
             ("&ni" ?\に)
             ("&nu" ?\ぬ)
             ("&ne" ?\ね)
             ("&no" ?\の)
             ("&ha" ?\は)
             ("&ba" ?\ば)
             ("&pa" ?\ぱ)
             ("&hi" ?\ひ)
             ("&bi" ?\び)
             ("&pi" ?\ぴ)
             ("&hu" ?\ふ)
             ("&bu" ?\ぶ)
             ("&pu" ?\ぷ)
             ("&he" ?\へ)
             ("&be" ?\べ)
             ("&pe" ?\ぺ)
             ("&ho" ?\ほ)
             ("&bo" ?\ぼ)
             ("&po" ?\ぽ)
             ("&ma" ?\ま)
             ("&mi" ?\み)
             ("&mu" ?\む)
             ("&me" ?\め)
             ("&mo" ?\も)
             ("&yA" ?\ゃ)
             ("&ya" ?\や)
             ("&yU" ?\ゅ)
             ("&yu" ?\ゆ)
             ("&yO" ?\ょ)
             ("&yo" ?\よ)
             ("&ra" ?\ら)
             ("&ri" ?\り)
             ("&ru" ?\る)
             ("&re" ?\れ)
             ("&ro" ?\ろ)
             ("&wA" ?\ゎ)
             ("&wa" ?\わ)
             ("&wi" ?\ゐ)
             ("&we" ?\ゑ)
             ("&wo" ?\を)
             ("&n5" ?\ん)
             ("&vu" ?\ゔ)
             ("&\"5" ?\゛)
             ("&05" ?\゜)
             ("&*5" ?\ゝ)
             ("&+5" ?\ゞ)
             ("&a6" ?\ァ)
             ("&A6" ?\ア)
             ("&i6" ?\ィ)
             ("&I6" ?\イ)
             ("&u6" ?\ゥ)
             ("&U6" ?\ウ)
             ("&e6" ?\ェ)
             ("&E6" ?\エ)
             ("&o6" ?\ォ)
             ("&O6" ?\オ)
             ("&Ka" ?\カ)
             ("&Ga" ?\ガ)
             ("&Ki" ?\キ)
             ("&Gi" ?\ギ)
             ("&Ku" ?\ク)
             ("&Gu" ?\グ)
             ("&Ke" ?\ケ)
             ("&Ge" ?\ゲ)
             ("&Ko" ?\コ)
             ("&Go" ?\ゴ)
             ("&Sa" ?\サ)
             ("&Za" ?\ザ)
             ("&Si" ?\シ)
             ("&Zi" ?\ジ)
             ("&Su" ?\ス)
             ("&Zu" ?\ズ)
             ("&Se" ?\セ)
             ("&Ze" ?\ゼ)
             ("&So" ?\ソ)
             ("&Zo" ?\ゾ)
             ("&Ta" ?\タ)
             ("&Da" ?\ダ)
             ("&Ti" ?\チ)
             ("&Di" ?\ヂ)
             ("&TU" ?\ッ)
             ("&Tu" ?\ツ)
             ("&Du" ?\ヅ)
             ("&Te" ?\テ)
             ("&De" ?\デ)
             ("&To" ?\ト)
             ("&Do" ?\ド)
             ("&Na" ?\ナ)
             ("&Ni" ?\ニ)
             ("&Nu" ?\ヌ)
             ("&Ne" ?\ネ)
             ("&No" ?\ノ)
             ("&Ha" ?\ハ)
             ("&Ba" ?\バ)
             ("&Pa" ?\パ)
             ("&Hi" ?\ヒ)
             ("&Bi" ?\ビ)
             ("&Pi" ?\ピ)
             ("&Hu" ?\フ)
             ("&Bu" ?\ブ)
             ("&Pu" ?\プ)
             ("&He" ?\ヘ)
             ("&Be" ?\ベ)
             ("&Pe" ?\ペ)
             ("&Ho" ?\ホ)
             ("&Bo" ?\ボ)
             ("&Po" ?\ポ)
             ("&Ma" ?\マ)
             ("&Mi" ?\ミ)
             ("&Mu" ?\ム)
             ("&Me" ?\メ)
             ("&Mo" ?\モ)
             ("&YA" ?\ャ)
             ("&Ya" ?\ヤ)
             ("&YU" ?\ュ)
             ("&Yu" ?\ユ)
             ("&YO" ?\ョ)
             ("&Yo" ?\ヨ)
             ("&Ra" ?\ラ)
             ("&Ri" ?\リ)
             ("&Ru" ?\ル)
             ("&Re" ?\レ)
             ("&Ro" ?\ロ)
             ("&WA" ?\ヮ)
             ("&Wa" ?\ワ)
             ("&Wi" ?\ヰ)
             ("&We" ?\ヱ)
             ("&Wo" ?\ヲ)
             ("&N6" ?\ン)
             ("&Vu" ?\ヴ)
             ("&KA" ?\ヵ)
             ("&KE" ?\ヶ)
             ("&Va" ?\ヷ)
             ("&Vi" ?\ヸ)
             ("&Ve" ?\ヹ)
             ("&Vo" ?\ヺ)
             ("&.6" ?\・)
             ("&-6" ?\ー)
             ("&*6" ?\ヽ)
             ("&+6" ?\ヾ)
             ("&b4" ?\ㄅ)
             ("&p4" ?\ㄆ)
             ("&m4" ?\ㄇ)
             ("&f4" ?\ㄈ)
             ("&d4" ?\ㄉ)
             ("&t4" ?\ㄊ)
             ("&n4" ?\ㄋ)
             ("&l4" ?\ㄌ)
             ("&g4" ?\ㄍ)
             ("&k4" ?\ㄎ)
             ("&h4" ?\ㄏ)
             ("&j4" ?\ㄐ)
             ("&q4" ?\ㄑ)
             ("&x4" ?\ㄒ)
             ("&zh" ?\ㄓ)
             ("&ch" ?\ㄔ)
             ("&sh" ?\ㄕ)
             ("&r4" ?\ㄖ)
             ("&z4" ?\ㄗ)
             ("&c4" ?\ㄘ)
             ("&s4" ?\ㄙ)
             ("&a4" ?\ㄚ)
             ("&o4" ?\ㄛ)
             ("&e4" ?\ㄜ)
             ("&eh4" ?\ㄝ)
             ("&ai" ?\ㄞ)
             ("&ei" ?\ㄟ)
             ("&au" ?\ㄠ)
             ("&ou" ?\ㄡ)
             ("&an" ?\ㄢ)
             ("&en" ?\ㄣ)
             ("&aN" ?\ㄤ)
             ("&eN" ?\ㄥ)
             ("&er" ?\ㄦ)
             ("&i4" ?\ㄧ)
             ("&u4" ?\ㄨ)
             ("&iu" ?\ㄩ)
             ("&v4" ?\ㄪ)
             ("&nG" ?\ㄫ)
             ("&gn" ?\ㄬ)
             ("&1c" ?\㈠)
             ("&2c" ?\㈡)
             ("&3c" ?\㈢)
             ("&4c" ?\㈣)
             ("&5c" ?\㈤)
             ("&6c" ?\㈥)
             ("&7c" ?\㈦)
             ("&8c" ?\㈧)
             ("&9c" ?\㈨)
             ("&10c" ?\㈩)
             ("&KSC" ?\㉿)
             ("&am" ?\㏂)
             ("&pm" ?\㏘)
             ("&\"3" ?\)
             ("&\"1" ?\)
             ("&\"!" ?\)
             ("&\"'" ?\)
             ("&\">" ?\)
             ("&\"?" ?\)
             ("&\"-" ?\)
             ("&\"(" ?\)
             ("&\"." ?\)
             ("&\":" ?\)
             ("&\"0" ?\)
             ("&\"," ?\)
             ("&\"_" ?\)
             ("&\"\"" ?\)
             ("&\";" ?\)
             ("&\"<" ?\)
             ("&\"=" ?\)
             ("&\"/" ?\)
             ("&\"p" ?\)
             ("&\"d" ?\)
             ("&\"i" ?\)
             ("&+_" ?\)
             ("&a+:" ?\)
             ("&Tel" ?\)
             ("&UA" ?\)
             ("&UB" ?\)
             ("&t3" ?\)
             ("&m3" ?\)
             ("&k3" ?\)
             ("&p3" ?\)
             ("&Mc" ?\)
             ("&Fl" ?\)
             ("&Ss" ?\)
             ("&Ch" ?\)
             ("&CH" ?\)
             ("&__" ?\)
             ("&/c" ?\)
             ("&ff" ?\ﬀ)
             ("&fi" ?\ﬁ)
             ("&fl" ?\ﬂ)
             ("&ffi" ?\ﬃ)
             ("&ffl" ?\ﬄ)
             ("&St" ?\ﬅ)
             ("&st" ?\ﬆ)
             ("&3+;" ?\ﹽ)
             ("&aM." ?\ﺂ)
             ("&aH." ?\ﺄ)
             ("&ah." ?\ﺈ)
             ("&a+-" ?\ﺍ)
             ("&a+." ?\ﺎ)
             ("&b+-" ?\ﺏ)
             ("&b+." ?\ﺐ)
             ("&b+," ?\ﺑ)
             ("&b+;" ?\ﺒ)
             ("&tm-" ?\ﺓ)
             ("&tm." ?\ﺔ)
             ("&t+-" ?\ﺕ)
             ("&t+." ?\ﺖ)
             ("&t+," ?\ﺗ)
             ("&t+;" ?\ﺘ)
             ("&tk-" ?\ﺙ)
             ("&tk." ?\ﺚ)
             ("&tk," ?\ﺛ)
             ("&tk;" ?\ﺜ)
             ("&g+-" ?\ﺝ)
             ("&g+." ?\ﺞ)
             ("&g+," ?\ﺟ)
             ("&g+;" ?\ﺠ)
             ("&hk-" ?\ﺡ)
             ("&hk." ?\ﺢ)
             ("&hk," ?\ﺣ)
             ("&hk;" ?\ﺤ)
             ("&x+-" ?\ﺥ)
             ("&x+." ?\ﺦ)
             ("&x+," ?\ﺧ)
             ("&x+;" ?\ﺨ)
             ("&d+-" ?\ﺩ)
             ("&d+." ?\ﺪ)
             ("&dk-" ?\ﺫ)
             ("&dk." ?\ﺬ)
             ("&r+-" ?\ﺭ)
             ("&r+." ?\ﺮ)
             ("&z+-" ?\ﺯ)
             ("&z+." ?\ﺰ)
             ("&s+-" ?\ﺱ)
             ("&s+." ?\ﺲ)
             ("&s+," ?\ﺳ)
             ("&s+;" ?\ﺴ)
             ("&sn-" ?\ﺵ)
             ("&sn." ?\ﺶ)
             ("&sn," ?\ﺷ)
             ("&sn;" ?\ﺸ)
             ("&c+-" ?\ﺹ)
             ("&c+." ?\ﺺ)
             ("&c+," ?\ﺻ)
             ("&c+;" ?\ﺼ)
             ("&dd-" ?\ﺽ)
             ("&dd." ?\ﺾ)
             ("&dd," ?\ﺿ)
             ("&dd;" ?\ﻀ)
             ("&tj-" ?\ﻁ)
             ("&tj." ?\ﻂ)
             ("&tj," ?\ﻃ)
             ("&tj;" ?\ﻄ)
             ("&zH-" ?\ﻅ)
             ("&zH." ?\ﻆ)
             ("&zH," ?\ﻇ)
             ("&zH;" ?\ﻈ)
             ("&e+-" ?\ﻉ)
             ("&e+." ?\ﻊ)
             ("&e+," ?\ﻋ)
             ("&e+;" ?\ﻌ)
             ("&i+-" ?\ﻍ)
             ("&i+." ?\ﻎ)
             ("&i+," ?\ﻏ)
             ("&i+;" ?\ﻐ)
             ("&f+-" ?\ﻑ)
             ("&f+." ?\ﻒ)
             ("&f+," ?\ﻓ)
             ("&f+;" ?\ﻔ)
             ("&q+-" ?\ﻕ)
             ("&q+." ?\ﻖ)
             ("&q+," ?\ﻗ)
             ("&q+;" ?\ﻘ)
             ("&k+-" ?\ﻙ)
             ("&k+." ?\ﻚ)
             ("&k+," ?\ﻛ)
             ("&k+;" ?\ﻜ)
             ("&l+-" ?\ﻝ)
             ("&l+." ?\ﻞ)
             ("&l+," ?\ﻟ)
             ("&l+;" ?\ﻠ)
             ("&m+-" ?\ﻡ)
             ("&m+." ?\ﻢ)
             ("&m+," ?\ﻣ)
             ("&m+;" ?\ﻤ)
             ("&n+-" ?\ﻥ)
             ("&n+." ?\ﻦ)
             ("&n+," ?\ﻧ)
             ("&n+;" ?\ﻨ)
             ("&h+-" ?\ﻩ)
             ("&h+." ?\ﻪ)
             ("&h+," ?\ﻫ)
             ("&h+;" ?\ﻬ)
             ("&w+-" ?\ﻭ)
             ("&w+." ?\ﻮ)
             ("&j+-" ?\ﻯ)
             ("&j+." ?\ﻰ)
             ("&y+-" ?\ﻱ)
             ("&y+." ?\ﻲ)
             ("&y+," ?\ﻳ)
             ("&y+;" ?\ﻴ)
             ("&lM-" ?\ﻵ)
             ("&lM." ?\ﻶ)
             ("&lH-" ?\ﻷ)
             ("&lH." ?\ﻸ)
             ("&lh-" ?\ﻹ)
             ("&lh." ?\ﻺ)
             ("&la-" ?\ﻻ)
             ("&la." ?\ﻼ)

             ("&(-" ?\∈)
             ("&(/" ?\∉)
             ("&cb" ?\•)
             ("&:)" ?\☺)
             ("&:(" ?\☹)
             ("&<3" ?\❤))))

(use-package mule-util
  :defer t
  :config (setq truncate-string-ellipsis "…"))

(use-package now-init
  :hook ((message-mode . now-remove-continuation-fringe-indicator)
         (prog-mode . now-comment-auto-fill-only-comments)
         (tabulated-list-mode
          . now-tabulated-list-mode-use-global-glyphless-char-display)
         ((nxml-mode text-mode) . now-set-fill-column-to-79)))

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
            (column-number-mode)))

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
                                                 nil nil nil nil nil))
                                      (semantix . ("nikolai.weibull@semantix.se"
                                                   "smtp.office365.com"
                                                   587
                                                   nil nil nil nil nil))
                                      (xtrf . ("xtrf@semantix.se"
                                               "smtp.office365.com"
                                               587
                                               nil nil nil nil nil))))
           (smtpmail-multi-associations '(("now@disu.se" disuse)
                                          ("nikolai.weibull@semantix.se" semantix)
                                          ("xtrf@semantix.se" xtrf)))))

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

(use-package undo-tree
  :no-require t
  :custom ((undo-tree-mode-lighter nil)
           (undo-tree-visualizer-timestamps t)))

(use-package undo-tree
  :no-require t
  :after evil
  :bind (:map evil-motion-state-map
              (", u" . undo-tree-visualize)
              ("U" . undo-tree-redo)))

(use-package vc-git
  :defer t
  :config (progn
            (defun vc-git-mode-line-string (file) "")))

(use-package vc-hooks
  :no-require t
  :custom ((vc-handled-backends nil)))

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
                               lines-tail
                               empty
                               indentation
                               space-before-tab)))
  :hook (((prog-mode text-mode) . whitespace-mode)
         (message-mode . whitespace-turn-off)))
(make-variable-buffer-local 'whitespace-line-column)
(setq-default whitespace-line-column 80)
(add-hook 'hack-local-variables-hook
          '(lambda ()
             (when (and whitespace-mode (not (= whitespace-line-column 80)))
               (whitespace-mode -1)
               (whitespace-mode))))

(use-package xdisp
  :no-require t
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
(evil-mode)
(load-theme 'now t)
(defun now-bug-reference-fontify-around (next &rest args)
  (let ((case-fold-search nil))
    (apply next args)))

(advice-add 'bug-reference-fontify :around 'now-bug-reference-fontify-around)
