;; -*- lexical-binding: t; -*-

(setq gc-cons-threshold 20000000
      undo-limit 80000000
      undo-strong-limit 120000000
      undo-outer-limit 360000000)

(require 'package)
(setf (alist-get "melpa" package-archives nil nil #'equal) "https://melpa.org/packages/")

(add-to-list 'load-path (concat user-emacs-directory "site-lisp"))
(require 'userloaddefs)

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

;; TODO Why?
(setq process-connection-type nil)

(defun now-report-emacs-startup-time ()
  "Write a ‘message’ that reports the time it took to start Emacs."
  (interactive)
  (message "Emacs ready in %.2f seconds"
           (float-time (time-subtract after-init-time
                                      before-init-time))))

(add-hook 'emacs-startup-hook #'now-report-emacs-startup-time)

;; TODO ctl-x-map "b" should be bound to counsel-ibuffer, but using
;; buff-menu-ext instead to generate the entries.
(define-key ctl-x-map "\C-b" 'buffer-menu)

(defun now-set-split-width-threshold-based-on-aspect-ratio (_)
  (setq split-width-threshold
        (pcase (cdr (assoc 'geometry (car (display-monitor-attributes-list))))
          ((and `(,_ ,_ ,width ,height) (guard (< width height))) nil)
          (_ 160))))

(add-to-list 'window-size-change-functions 'now-set-split-width-threshold-based-on-aspect-ratio)

(eval-and-compile
  (require 'use-package)
  (setq use-package-compute-statistics nil
        use-package-verbose nil))

;; This comes first, as we want path set early on.
(use-package now-path
  :when (eq (window-system) 'ns))

(use-package ace-window
  :ensure t
  :bind (("C-x C-o" . ace-window)
         ("C-x o" . ace-window)))

(use-package amx
  :ensure t
  :config (amx-mode 1))

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

(use-package avy
  :bind* (("C-." . avy-goto-char-timer)
          ("C->" . avy-goto-line))
  :custom ((avy-timeout-seconds 0.3))
  ;; TODO Probably use own setup code here instead.
  :config (avy-setup-default))

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
               ("r" . Buffer-menu-toggle-read-only))))

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
           (calendar-week-start-day 1)))

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
           (compilation-error-regexp-alist '(sbt
                                             maven
                                             clang-include
                                             gcc-include
                                             typescript-X
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
  :hook ((text-mode . display-fill-column-indicator-mode)))

(use-package docfold
  :hook ((prog-mode . docfold-minor-mode)))

(use-package ediff
  :no-require t
  :custom ((ediff-window-setup-function 'ediff-setup-windows-plain)
           (ediff-split-window-function 'split-window-horizontally)))

(use-package eglot
  :ensure t
  :custom ((eglot-ignored-server-capabilites '(:documentHighlightProvider))
           (eglot-workspace-configuration '((:gopls . ((allowImplicitNetworkAccess . t)
                                                       (staticcheck . t)))))))

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

(use-package ffap
  :custom ((ffap-machine-p-known 'accept)))

(use-package files
  :custom ((make-backup-files nil)
           (require-final-newline t)
           (revert-without-query '("\\.log$")))
  :defer t
  :config (progn
            (setq insert-directory-program "a")))

(use-package find-func
  :defer t
  :config (progn
            (setq find-function-C-source-directory "~/Projects/emacs/src")))

(use-package fira-code-mode
  :ensure t
  :diminish fira-code-mode
  :custom ((fira-code-mode-disabled-ligatures '("[]" "{-" "lambda" "x")))
  :hook ((prog-mode . fira-code-mode)))

;; TODO Customize
(use-package flycheck
  :disabled)

(use-package flyspell
  :hook ((message-mode . flyspell-mode)))

(use-package forge
  :ensure t
  :after magit)

(use-package frame
  :custom ((blink-cursor-blinks 0)
           (default-frame-alist '((right-fringe . 0))))
  :config (blink-cursor-mode))

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

(use-package gxref
  :ensure t)

(use-package hide-mode-line
  :config (hide-mode-line-mode)
  :bind (:map ctl-x-map
              ("," . hide-mode-line-show-mode-line)))

(use-package highlight-selected-window
  :config (highlight-selected-window-mode))

(use-package hl-line
  :hook (((Buffer-menu-mode
            arc-mode
            dired-mode
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
                                      holiday-solar-holidays))))

(use-package info
  :config (progn
            (add-to-list 'Info-additional-directory-list
                         (concat user-emacs-directory "info"))))

(use-package isearch
  :no-require t
  :custom ((search-whitespace-regexp "[ \t\r\n]+")))

(use-package iso-transl
  :defer t
  :config (progn
            (iso-transl-define-keys (mapcar (lambda (p) (cons (car p) nil)) iso-transl-char-map))
            (setq iso-transl-dead-key-alist nil)
            (defun iso-transl-define-keys (alist)
              (while alist
                (let ((translated-vec (cdr (car alist))))
                  (define-key iso-transl-ctl-x-8-map (car (car alist)) translated-vec))
                (setq alist (cdr alist))))
            (iso-transl-define-keys iso-transl-char-map)
            (setf (alist-get "now" iso-transl-language-alist nil nil #'equal)
                  '((".")
                    (".." . [?·])
                    (".3" . [?…])
                    ("*'" . [?ʹ])
                    ("*\"" . [?ʺ])))
            (iso-transl-set-language "now")))

(use-package ispell
  :no-require t
  :custom ((ispell-local-dictionary-alist
            '((nil "[[:alpha:]]" "[^[:alpha:]]" "['’]" nil ("-B") nil utf-8)
              ("en_US" "[[:alpha:]]" "[^[:alpha:]]" "['’]" nil ("-B") nil utf-8)
              ("en_GB-ise" "[[:alpha:]]" "[^[:alpha:]]" "['’]" nil ("-B") nil utf-8)
              ("sv_SE" "[[:alpha:]]" "[^[:alpha:]]" "['’]" nil ("-C") nil utf-8)))))

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

(use-package jka-cmpr-hook
  :no-require t
  :custom ((jka-compr-verbose . nil)))

(use-package js
  :no-require t
  :mode (("\\.jsx\\(inc\\)?\\'" . js-mode))
  :custom ((js-indent-level 2)))

(use-package json-mode
  :ensure t)

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
  :bind ((:map ctl-x-map
               ("G" . magit-file-dispatch))))

(use-package make-mode
  :no-require t
  :custom ((makefile-backslash-align nil)))

(use-package now-make-mode
  :hook (makefile-mode . now-make-mode-remove-space-after-tab-from-whitespace-style))

(use-package man
  :custom ((Man-notify-method 'pushy)))

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
  :hook (((compilation-mode help-mode) . now-do-not-show-trailing-whitespace)
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

(use-package package
  :no-require t
  :custom ((package-quickstart t)))

(use-package page
  :no-require t
  :config (put 'narrow-to-page 'disabled nil))

(use-package paredit
  :ensure t
  :diminish paredit-mode
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

(use-package now-project
  :no-require t
  :bind (:map ctl-x-map
              ("c" . now-project-display-compilation)))

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
  :custom ((completion-show-help nil)
           (indent-tabs-mode nil)
           (what-cursor-show-names t))
  :hook (((prog-mode text-mode) . auto-fill-mode)
         ((go-mode nxml-mode) . turn-off-auto-fill))
  :defer nil
  :bind (:map global-map
              ("s-z" . 'undo-only)
              ("s-Z" . 'undo-redo))
  :config (progn
            (line-number-mode -1)))

(use-package smerge-mode
  :custom ((smerge-auto-leave nil)
           (smerge-command-prefix "\C-cv")))

(use-package smie
  :defer t
  :config (progn
            (advice-add 'smie-auto-fill :around 'now-smie-auto-fill)))

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

(use-package solar
  :no-require t
  :custom ((calendar-time-display-form '(24-hours ":" minutes (if time-zone " ")
                                                  time-zone))
           (calendar-latitude 57.708870)
           (calendar-longitude 11.97456)
           (calendar-location-name "Göteborg")))

(use-package sql-indent
  :ensure t
  :custom ((sqlind-basic-offset 8)))

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
  :no-require t)

(use-package undo-tree
  :disabled
  :custom ((undo-tree-mode-lighter nil)
           (undo-tree-visualizer-timestamps t))
  :config (global-undo-tree-mode))

(use-package vc-git
  :defer t
  :config (progn
            (defun vc-git-mode-line-string (_) "")))

(use-package vc-hooks
  :defer t
  :custom ((vc-handled-backends '(Git))))

(use-package visual-fill-column
  :ensure t
  :hook ((message-mode . visual-fill-column-mode)))

(use-package whitespace
  :diminish (global-whitespace-mode whitespace-mode)
  :custom ((whitespace-global-modes '(prog-mode))
           (whitespace-style '(face
                               missing-newline-at-eof
                               empty
                               indentation
                               space-after-tab
                               space-before-tab)))
  :config (global-whitespace-mode))

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

(use-package yaml-mode
  :ensure t)

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
(load-theme 'now t)

(defun now-bug-reference-fontify-around (next &rest args)
  (let ((case-fold-search nil))
    (apply next args)))

(advice-add 'bug-reference-fontify :around 'now-bug-reference-fontify-around)
