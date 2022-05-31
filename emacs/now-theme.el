;; -*- lexical-binding: t; -*-

(deftheme now
  "Color theme by Nikolai Weibull.")

(cl-macrolet ((value (variable-name)
            `'(if (boundp ',variable-name)
                ,variable-name
              (custom--standard-value ',variable-name))))
  (custom-theme-set-variables
   'now

   ;; Editing

   ;;   Editing Basics
   '(show-trailing-whitespace t)
   '(what-cursor-show-names t)

   ;;   Matching
   '(replace-lax-whitespace t)

   ;;     Company
   '(company-backends
     '(company-bbdb
       company-semantic
       company-cmake
       company-capf
       company-clang
       company-files
       (company-dabbrev-code
        company-gtags
        company-keywords)
       company-dabbrev))
   '(company-idle-delay .175)
   '(company-show-numbers t)
   '(global-company-mode t)

   ;;       Company Dabbrev
   '(company-dabbrev-downcase nil)
   '(company-dabbrev-ignore-case nil)

   ;;     Completion
   '(completion-show-help nil)

   ;;     Isearch
   '(search-whitespace-regexp "[ \t\r\n]+")

   ;;     Paren Showing
   '(show-paren-delay 0)
   '(show-paren-mode t)

   ;;   Undo
   '(undo-limit 80000000)
   '(undo-strong-limit 120000000)
   '(undo-outer-limit 360000000)

   ;; Convenience

   ;;   Avy
   '(avy-timeout-seconds 0.3)

   ;;   Ffap
   '(ffap-machine-p-known 'accept)

   ;;   Tab Bar
   '(tab-bar-show nil)

   ;;   Whitespace
   '(whitespace-global-modes '(prog-mode))
   '(whitespace-style
     '(face
       missing-newline-at-eof
       empty
       indentation
       space-after-tab
       space-before-tab))
   '(global-whitespace-mode t)

   ;; Files

   ;;   Auto Insert
   '(auto-insert-alist
     (assoc-delete-all
      nil
      auto-insert-alist
      (lambda (key _)
        (or (member
             (cdr-safe key)
             '("C / C++ header"
               "C / C++ program"
               "Makefile"))
            (member
             key
             '(html-mode
               plain-tex-mode
               bibtex-mode
               latex-mode
               ada-mode))))))
   '(auto-insert-mode t)

   ;;   Backup
   '(make-backup-files nil)
   '(require-final-newline t)

   ;;   Dired
   '(dired-dwim-target t)
   '(dired-isearch-filenames 'dwim)
   '(dired-listing-switches "--si -al")
   `(dired-mode-hook (cons 'hl-line-mode ,(value dired-mode-hook)))
   '(dired-recursive-copies 'always)
   '(dired-recursive-deletes 'always)

   ;;   Find File
   '(revert-without-query '("\\.log$"))

   ;; Text
   `(text-mode-hook
     (cl-list*
      'auto-fill-mode
      'display-fill-column-indicator-mode
      'now-set-fill-column-to-80
      ,(value text-mode-hook)))

   ;;   Markdown
   '(markdown-list-item-bullets '("•" "◦" "‣"))

   ;;   Relax Ng
   `(rng-schema-locating-files
     (cons (concat user-emacs-directory "etc/schema/schemas.xml")
           ,(value rng-schema-locating-files)))

   ;; Data

   ;;   Compression

   ;;     Jka Compr
   '(jka-compr-verbose nil)

   ;; Communication

   ;;   Bug Reference
   '(bug-reference-bug-regexp "\\(\\b\\([A-Z]\\{3,\\}-[0-9]+\\)\\b\\)")

   ;; Programming

   ;;   Languages

   ;;     C
   `(c-default-style
     (cl-list*
      '(java-mode . "now-java") '(other . "now-c") ,(value c-default-style)))
   '(c-electric-pound-behavior '(alignleft))
   `(c-mode-hook
     (cl-list*
      'now-c-set-adaptive-fill-function
      'now-c-auto-newline-mode
      ,(value c-mode-hook)))

   ;;     Go
   `(go-mode-hook
     (cl-list*
      'now-set-tab-width-to-2 'turn-off-auto-fill ,(value go-mode-hook)))

   ;;     Nxml
   '(nxml-char-ref-display-glyph-flag nil)
   '(nxml-slash-auto-complete-flag t)

   ;;     Prog Mode
   `(prog-mode-hook
     (cl-list*
      'auto-fill-mode
      'fira-code-mode
      'now-comment-auto-fill-only-comments
      'now-set-fill-column-to-80
       ,(value prog-mode-hook)))

   ;;     Sh

   ;;       Sh Script
   `(sh-alias-alist
     (cl-list* '(@SHELL@ . sh) '(@ZSHELL@ . zsh) ,(value sh-alias-alist)))

   ;;         Sh Indentation
   '(sh-basic-offset 2)

   ;;   Tools

   ;;     Compilation
   '(compilation-error-regexp-alist
     (sbt
      maven
      clang-include
      gcc-include
      typescript-X
      gmake
      gnu
      gcov-file
      gcov-header
      gcov-nomark
      gcov-called-line
      gcov-never-called))
   `(compilation-mode-hook
     (cons 'now-do-not-show-trailing-whitespace ,(value compilation-mode-hook)))
   '(compilation-scroll-output 'first-error)

   ;;     Grep
   '(grep-highlight-matches t)
   '(grep-program "g")
   '(grep-use-null-device nil)
   '(grep-use-null-filename-separator t)

   ;;     Magit

   ;;       Magit Essentials
   '(magit-repository-directories '(("~/Projects" . 1)))

   ;;       Magit Modes

   ;;         Git Commit
   `(git-commit-setup-hook
     (cons 'now-set-fill-column-to-72 ,(value git-commit-setup-hook)))

   ;;         Magit Blame
   `(magit-blame-styles
     (let ((styles ,(value magit-blame-styles)))
       (cons
        (cons
         'headings
         (cons
          '(heading-format . "%-20a %C %.6H %s\n")
          (cdr (assq 'headings styles))))
        styles)))

   ;;     Makefile
   '(makefile-backslash-align nil)
   `(makefile-mode-hook
     (cons
      'now-remove-space-after-tab-from-whitespace-style
      ,(value makefile-mode-hook)))

   ;;     Smerge
   '(smerge-auto-leave nil)
   '(smerge-command-prefix "\C-cv")

   ;;     Vc
   '(vc-handled-backends '(Git))

   ;; Applications

   ;;   Calendar
   '(calendar-date-style iso)
   '(calendar-intermonth-text
     '(propertize
       (format
        "%2d"
        (car
         (calendar-iso-from-absolute
          (calendar-absolute-from-gregorian
           (list month day year)))))
       'font-lock-face 'week))
   '(calendar-latitude 57.708870)
   '(calendar-location-name "Göteborg")
   '(calendar-longitude 11.97456)
   '(calendar-mark-holidays-flag t)
   '(calendar-time-display-form
     '(24-hours ":" minutes (if time-zone " ") time-zone))
   '(calendar-week-start-day 1)

   ;;     Eglot
   '(eglot-ignored-server-capabilites '(:documentHighlightProvider))
   '(eglot-workspace-configuration
     '((:gopls . ((allowImplicitNetworkAccess . t) (staticcheck . t)))))

   ;;     Holidays
   '(calendar-holidays
     (append holiday-general-holidays
             holiday-local-holidays
             holiday-christian-holidays
             holiday-other-holidays
             holiday-solar-holidays))
   '(holiday-christian-holidays
     '((holiday-easter-etc -2 "Good Friday")
       (holiday-easter-etc +1 "Easter Monday")
       (holiday-easter-etc +39 "Ascension Day")
       (holiday-easter-etc +49 "Pentecost")
       (holiday-fixed 12 24 "Christmas Eve")
       (holiday-fixed 12 25 "Christmas")
       (holiday-fixed 12 26 "Boxing Day")))
   '(holiday-general-holidays
     '((holiday-fixed 1 1 "New Year’s Day")
       (holiday-fixed 5 1 "International Worker’s Day")
       (holiday-fixed 6 6 "National Day of Sweden")
       (holiday-float 6 5 -1 "Midsummer’s Eve" 26)
       (holiday-fixed 12 31 "New Year’s Eve")))

   ;;   Ispell
   ;; TODO This should perhaps go in another theme, now-configuration?
   '(ispell-local-dictionary-alist
     '((nil "[[:alpha:]]" "[^[:alpha:]]" "['’]" nil ("-B") nil utf-8)
       ("en_US" "[[:alpha:]]" "[^[:alpha:]]" "['’]" nil ("-B") nil utf-8)
       ("en_GB-ise" "[[:alpha:]]" "[^[:alpha:]]" "['’]" nil ("-B") nil utf-8)
       ("sv_SE" "[[:alpha:]]" "[^[:alpha:]]" "['’]" nil ("-C") nil utf-8)))

   ;;   Mail

   ;;     Message

   ;;       Message Various
   `(message-mode-hook
     (cl-list*
      'flyspell-mode
      'now-remove-continuation-fringe-indicator
      'visual-fill-column-mode
      ,(value message-mode-hook)))

   ;;   Package
   '(package-quickstart t)

   ;;   Mail

   ;;     Gnus

   ;;       Auth Source
   '(auth-sources
     (pcase system-type
       ('darwin '(macos-keychain-internet))
       ('gnu/linux '("secrets:Login"))))

   ;; Development

   ;;   Extensions

   ;;     Eldoc
   '(eldoc-echo-area-use-multiline-p nil)

   ;;   Internal

   ;;     Storage Allocation
   '(gc-cons-threshold 20000000)

   ;;   Lisp
   `(emacs-lisp-mode-hook
     (cl-list*
      'eldoc-mode
      'now-set-page-delimiter-to-three-semicolons
      ,(value emacs-lisp-mode-hook)))

   ;;     Checkdoc
   '(checkdoc-arguments-in-order-flag t)
   '(checkdoc-package-keywords-flag t)
   '(checkdoc-spellcheck-documentation-flag t)

   ;; Environment

   ;;   Frames
   '(default-frame-alist
     '((cursor-type . bar) (height . 1.0) (right-fringe . 0) (width . 0.5)))
   '(initial-frame-alist '((vertical-scroll-bars)))
   '(menu-bar-mode (eq window-system 'ns))
   '(scroll-bar-mode nil)
   '(tool-bar-mode nil)
   '(undelete-frame-mode t)

   ;;     Cursor
   '(blink-cursor-blinks 0)
   '(blink-cursor-mode t)

   ;;     Destkop
   `(desktop-after-read-hook
     (cons 'desktop-auto-save-enable ,(value desktop-after-read-hook)))
   '(desktop-base-file-name "emacs.desktop")
   '(desktop-globals-to-save
     '(command-history
       compile-history
       desktop-missing-file-warning
       extended-command-history
       file-name-history
       log-edit-comment-ring
       minibuffer-history
       read-expression-history
       regexp-search-ring
       register-alist
       search-ring
       shell-command-history
       tags-file-name
       tags-table-list))
   '(desktop-lazy-idle-delay 2)
   '(desktop-lazy-verbose nil)
   '(desktop-restore-eager 0)
   '(desktop-save-mode t)

   ;;      Ediff Window
   '(ediff-window-setup-function 'ediff-setup-windows-plain)
   '(ediff-split-window-function 'split-window-horizontally)

   ;;      Highlight Selected Window
   '(highlight-selected-window-mode t)

   ;;   Minibuffer
   '(completions-format 'vertical)

   ;;   Mode Line
   '(hide-mode-line-mode t)
   '(line-number-mode nil)

   ;;   Mouse
   '(mouse-wheel-mode nil)

   ;; Faces

   ;;   Fira Code Ligatures
   '(fira-code-mode-disabled-ligatures '("[]" "{-" "lambda" "x"))

   ;; Help
   `(help-mode-hook
     (cons 'now-do-not-show-trailing-whitespace ,(value help-mode-hook)))

   ;;   Customize

   ;;     Customize Buffer
   '(custom-search-field nil)

   ;; Unknown
   '(face-font-family-alternatives
     '(("Monospace" "SF Mono" "DejaVu Sans Mono" "fixed")
       ("Monospace Serif" "SF Mono" "DejaVu Sans Mono" "fixed")
       ("Sans Serif" "DejaVu Sans" "fixed")))
   '(magit-diff-highlight-hunk-region-functions
     '(magit-diff-highlight-hunk-region-dim-outside
       magit-diff-highlight-hunk-region-using-face))))

(let* ((cui '((class color) (min-colors 89)))
       (gtk '((type gtk)))
       (gui '((type graphic)))
       (dark '((background dark)))
       (lite '((background light)))
       ;; (cui-dark `(,@cui ,@dark))
       ;; (gtk-dark `(,@gtk ,@dark))
       (gui-dark `(,@gui ,@dark))
       ;; (cui-lite `(,@cui ,@lite))
       ;; (gtk-lite `(,@gtk ,@lite))
       (gui-lite `(,@gui ,@lite))
       (black "#000000")
       (red "#951616")
       (green "#257325")
       (blue "#2f5a9b")
       ;; (mustard "#766020")
       (purple "#602f80")
       (cyan "#5694a8")
       (light-blue "#2080c0")
       (off-black "#1e1e1e")
       (off-white "#dedede")
       (light-red "#f02626")
       (light-green "#009000")
       (light-yellow "#f0a500")
       ;; (light-magenta "#933763")
       (light-cyan "#80b0c0")
       (brown "#af5f00")
       (off-red "RosyBrown1")
       ;; (light-orange "#ffaf5f")
       (lighter-yellow "#ffd700")
       (white "#ffffff")
       (yellow "#ffbd00")
       ;; macOS
       (selection-dark "#1c5ac8")
       ;; macOS
       ;; (selection-lite "#2165d9")
       )
  (custom-theme-set-faces
    'now
    `(avy-lead-face ((,cui (:foreground ,black :background ,off-red))))
    `(avy-lead-face-0 ((,cui (:inherit avy-lead-face))))
    `(avy-lead-face-1 ((,cui (:inherit avy-lead-face))))
    `(avy-lead-face-2 ((,cui (:inherit avy-lead-face))))
    `(buffer-menu-buffer ((,cui)))
    `(compilation-column-number ((,cui (:inherit compilation-line-number))))
    `(compilation-line-number ((,cui)))
    `(compilation-mode-line-exit ((,cui (:inherit compilation-info))))
    `(compilation-mode-line-fail ((,cui (:inherit compilation-error))))
    `(cursor ((,gui-dark (:foreground ,off-black :background ,off-white))
              (,gui-lite (:foreground ,white :background ,black))))
    `(custom-button ((,gui (:box (:line-width 1 :color "#656a6f")
                            :background "#e9ebed" :foreground ,black))))
    `(custom-button-mouse ((,gui (:box (:line-width 1 :color "#478cba")
                                  :background "#e9ebed" :foreground ,black))))
    `(custom-button-pressed ((,gui (:box (:line-width 1 :color "#35576d")
                                    :background "#d4d6da" :foreground ,black))))
    `(custom-group-tag ((,cui (:inherit variable-pitch :height 1.2 :weight bold))))
    `(custom-state ((,cui)))
    `(custom-variable-tag ((,cui (:inherit variable-pitch
                                  :weight bold
                                  :box (:line-width 1)))))
    `(default ((default . (:family "Fira Code" :height 120))
               (,gtk . (:height 105))
               (,gui-dark . (:foreground ,off-white :background ,off-black))
               (,gui-lite . (:foreground ,black :background ,white))))
    `(diff-added ((,cui (:foreground ,green))))
    `(diff-context ((,cui (:foreground "grey50"))))
    `(diff-file-header ((,cui (:inherit diff-header))))
    `(diff-header ((,cui (:inherit diff-context :weight bold))))
    `(diff-indicator-added ((,cui (:inherit diff-added))))
    `(diff-indicator-removed ((,cui (:inherit diff-removed))))
    `(diff-nonexistent ((,cui (:foreground ,red))))
    `(diff-refine-added ((,cui (:inherit (diff-added diff-refine-change)))))
    `(diff-refine-removed ((,cui (:inherit (diff-removed diff-refine-change)))))
    `(diff-removed ((,cui (:foreground ,red))))
    `(dired-directory ((,cui (:foreground ,blue))))
    `(dired-header ((,cui (:inherit header))))
    `(dired-perm-write ((,cui (:foreground "#ff00ff"))))
    `(dired-symlink ((,cui (:foreground ,light-cyan))))
    `(eglot-diagnostic-tag-unnecessary-face ((,cui (:inherit font-lock-warning-face))))
    `(error ((,cui (:foreground ,white :background ,red))))
    `(escape-glyph ((,cui (:foreground ,light-blue))))
    `(flymake-note ((,cui)))
    `(flymake-warning ((,cui (:underline (:style wave :color ,yellow)))))
    `(flyspell-duplicate ((,cui (:foreground ,yellow :underline t))))
    `(flyspell-incorrect ((,cui (:foreground ,red :underline t))))
    `(font-lock-builtin-face ((,cui)))
    `(font-lock-comment-face ((,cui)))
    `(font-lock-constant-face ((,cui)))
    `(font-lock-doc-face ((,cui (:inherit font-lock-comment-face))))
    `(font-lock-function-name-face ((,cui)))
    `(font-lock-keyword-face ((,cui)))
    `(font-lock-preprocessor-face ((,cui)))
    `(font-lock-regexp-grouping-backslash ((,cui)))
    `(font-lock-regexp-grouping-construct ((,cui)))
    `(font-lock-string-face ((,cui)))
    `(font-lock-type-face ((,cui)))
    `(font-lock-variable-name-face ((,cui)))
    `(font-lock-warning-face ((,cui (:inherit warning))))
    `(fringe ((,gui-dark (:background "grey20"))
              (,gui-lite (:background ,white))))
    `(git-commit-branch-face ((,cui (:inherit font-lock-comment-face))))
    `(git-commit-comment-heading-face ((,cui)))
    `(git-commit-comment-file-face ((,cui)))
    `(glyphless-char ((,cui (:inherit escape-glyph))))
    `(gnus-summary-normal-ancient ((,cui)))
    `(gnus-summary-normal-read ((,cui)))
    `(gnus-summary-normal-ticked ((,cui)))
    `(gnus-summary-normal-unread ((,cui (:weight bold))))
    `(header-line ((,cui (:inherit mode-line))))
    `(highlight ((,cui (:foreground ,white :background ,selection-dark))))
    `(highlight-selected-window-unselected-window ((,gui-dark :background "grey15" :extend t)
						   (,gui-lite :background "grey90" :extend t)))
    `(holiday ((,cui (:foreground ,white :background ,red))))
    `(ido-first-match ((,cui (:inherit isearch))))
    `(ido-only-match ((,cui (:inherit success))))
    `(ido-subdir ((,cui (:inherit dired-directory))))
    `(ido-virtual ((,cui (:foreground "grey50"))))
    `(info-menu-header ((,cui)))
    `(info-menu-star ((,cui)))
    `(info-title-1 ((,cui)))
    `(info-title-2 ((,cui)))
    `(info-title-3 ((,cui)))
    `(info-title-4 ((,cui)))
    `(info-node ((,cui)))
    `(isearch ((,cui (:foreground ,black :background ,lighter-yellow))))
    `(isearch-fail ((,cui (:inherit error))))
    `(isearch-group-1 ((,cui (:inherit underline))))
    `(isearch-group-2 ((,cui (:inherit underline))))
    `(ivy-current-match ((,cui (:inherit highlight))))
    `(ivy-cursor ((,cui (:inherit cursor))))
    `(ivy-highlight-face ((,cui (:foreground ,blue))))
    `(ivy-match-required-face ((,cui (:inherit error))))
    `(ivy-minibuffer-match-face-1 ((,cui (:inherit isearch))))
    `(ivy-minibuffer-match-face-2 ((,cui (:inherit isearch))))
    `(ivy-minibuffer-match-face-3 ((,cui (:inherit isearch))))
    `(ivy-minibuffer-match-face-4 ((,cui (:inherit isearch))))
    `(ivy-remote ((,cui)))
    `(ivy-virtual ((,cui (:foreground "grey50"))))
    `(lazy-highlight ((,cui (:foreground ,black :background ,light-yellow))))
    `(link ((,cui (:foreground ,blue))))
    `(link-visited ((,cui (:foreground ,purple))))
    `(magit-blame-heading ((,cui (:inherit magit-blame-highlight))))
    `(magit-branch ((,cui (:inherit dired-directory))))
    `(magit-diff-added ((,cui (:inherit diff-added))))
    `(magit-diff-added-highlight ((,cui (:foreground ,light-green
                                         :background ,blue
                                         :extend t))))
    `(magit-diff-context ((,cui (:inherit diff-context))))
    `(magit-diff-context-highlight ((,cui (:foreground "grey70"
                                           :background ,blue
                                           :extend t))))
    `(magit-diff-file-heading-selection ((,cui (:inherit region :extend t))))
    `(magit-diff-hunk-heading ((,cui (:inherit diff-hunk-header))))
    `(magit-diff-hunk-heading-highlight ((,cui (:inherit magit-diff-hunk-heading))))
    `(magit-diff-hunk-heading-selection ((,cui (:inherit region :extend t))))
    `(magit-diff-hunk-region ((,cui (:inherit region :extend t))))
    `(magit-diff-removed ((,cui (:inherit diff-removed))))
    `(magit-diff-removed-highlight ((,cui (:foreground ,light-red
                                           :background ,blue
                                           :extend t))))
    `(magit-diffstat-added ((,cui (:inherit diff-added))))
    `(magit-diffstat-removed ((,cui (:inherit diff-removed))))
    `(magit-filename ((,cui)))
    `(magit-log-author ((,cui)))
    `(magit-log-date ((,cui)))
    `(magit-log-head-label-bisect-bad ((,cui (:inherit error))))
    `(magit-log-head-label-bisect-good ((,cui (:inherit success))))
    `(magit-log-head-label-default ((,cui (:background "grey90"))))
    `(magit-log-head-label-local ((,cui (:foreground ,light-blue
                                           :background "grey90"))))
    `(magit-log-head-label-remote ((,cui (:foreground ,green
                                            :background "grey90"))))
    `(magit-log-head-label-tags ((,cui (:foreground ,brown
                                          :background "grey90"))))
    `(magit-log-sha1 ((,cui)))
    `(magit-process-ng ((,cui (:inherit error))))
    `(magit-process-ok ((,cui)))
    `(magit-section-heading ((,cui (:weight bold))))
    `(magit-section-heading-selection ((,cui (:inherit region :extend t))))
    `(magit-section-highlight ((,cui (:inherit highlight :extend t))))
    `(makefile-space ((,cui (:inherit error))))
    `(markdown-blockquote-face ((,cui (:inherit markdown-code-face))))
    `(markdown-code-face ((,cui)))
    `(markdown-list-face ((,cui)))
    `(markdown-markup-face ((,cui)))
    `(match ((,cui (:inherit isearch))))
    `(message-cited-text ((,cui (:inherit font-lock-comment-face))))
    `(message-header-cc ((,cui)))
    `(message-header-name ((,cui)))
    `(message-header-other ((,cui)))
    `(message-header-subject ((,cui)))
    `(message-header-to ((,cui)))
    `(minibuffer-prompt ((,cui)))
    `(mode-line ((,cui)))
    `(mode-line-buffer-id ((,cui)))
    `(mode-line-highlight ((,cui (:inherit link))))
    `(mode-line-inactive ((,cui)))
    `(mu4e-compose-separator-face ((,cui)))
    `(mu4e-flagged-face ((,cui)))
    `(mu4e-header-highlight-face ((,cui (:inherit highlight))))
    `(mu4e-header-key-face ((,cui)))
    `(mu4e-trashed-face ((,cui)))
    `(mu4e-unread-face ((,cui)))
    `(ned-info-on-file-mode ((,cui (:foreground ,green))))
    `(ned-info-on-file-read-only ((,cui (:foreground ,red))))
    `(org-agenda-date-weekend ((,cui)))
    `(org-agenda-done ((,cui)))
    `(org-agenda-restriction-lock ((,cui)))
    `(org-agenda-structure ((,cui (:weight bold))))
    `(org-block ((,cui (:foreground "grey40"))))
    `(org-code ((,cui (:foreground "grey40"))))
    `(org-column ((,cui (:foreground ,black))))
    `(org-date ((,cui (:inherit font-lock-comment-face))))
    `(org-date-selected ((,cui (:inherit region))))
    `(org-delegated ((,cui (:inherit org-waiting))))
    `(org-done ((,cui (:foreground ,white :background "#14892c"))))
    `(org-hold ((,cui (:background "grey70"))))
    `(org-hide ((,cui (:foreground ,white))))
    `(org-mode-line-clock ((,cui)))
    `(org-mode-line-clock-overrun ((,cui (:inherit warning))))
    `(org-next ((,cui (:foreground "#403000" :background "#ffd351"))))
    `(org-priority-a ((,cui (:weight bold))))
    `(org-priority-b ((,cui)))
    `(org-priority-c ((,cui (:foreground "grey40"))))
    `(org-scheduled-today ((,cui)))
    `(org-table ((,cui)))
    `(org-tag ((,cui)))
    `(org-todo ((,cui (:foreground ,white :background "#4a6785"))))
    `(org-waiting ((,cui (:background ,yellow))))
    `(outline-1 ((,cui)))
    `(outline-2 ((,cui)))
    `(outline-3 ((,cui)))
    `(outline-4 ((,cui)))
    `(outline-5 ((,cui)))
    `(outline-6 ((,cui)))
    `(outline-7 ((,cui)))
    `(outline-8 ((,cui)))
    `(region ((,cui (:background ,light-cyan))))
    `(scala-font-lock:var-face ((,cui (:inherit font-lock-variable-name-face))))
    `(secondary-selection ((,cui (:background ,cyan))))
    `(sh-heredoc ((,cui (:inherit font-lock-string-face))))
    `(sh-quoted-exec ((,cui (:inherit font-lock-string-face))))
    `(show-paren-match ((,cui (:inherit success))))
    `(show-paren-mismatch ((,cui (:inherit error))))
    `(success ((,cui (:foreground ,white :background ,green))))
    `(trailing-whitespace ((,cui (:inherit error))))
    `(typescript-jsdoc-tag ((,cui (:inherit font-lock-comment-face))))
    `(typescript-jsdoc-type ((,cui (:inherit font-lock-comment-face))))
    `(typescript-jsdoc-value ((,cui (:inherit font-lock-comment-face))))
    `(undo-tree-visualizer-active-branch-face ((t)))
    `(undo-tree-visualizer-current-face ((t)))
    `(undo-tree-visualizer-unmodified-face ((,cui (:foreground ,green))))
    `(warning ((,cui (:foreground ,white :background ,yellow))))
    `(week ((,cui (:inherit font-lock-comment-face))))
    `(whitespace-empty ((,cui (:inherit whitespace-trailing))))
    `(whitespace-hspace ((,cui (:inherit whitespace-space))))
    `(whitespace-indentation ((,cui (:inherit whitespace-space))))
    `(whitespace-line ((,cui (:inherit warning))))
    `(whitespace-newline ((,cui (:inherite whitespace-space))))
    `(whitespace-space ((,cui (:foreground ,light-blue))))
    `(whitespace-space-after-tab ((,cui (:inherit whitespace-trailing))))
    `(whitespace-space-before-tab ((,cui (:inherit whitespace-trailing))))
    `(whitespace-tab ((,cui (:inherit whitespace-space))))
    `(whitespace-trailing ((,cui (:inherit trailing-whitespace))))
    `(widget-field ((,gui (:box (:line-width 1 :color "#656a6f")))
                    (,cui (:background ,off-white))))
    `(woman-bold ((,gui (:weight bold)) (,cui (:foreground ,blue))))
    `(woman-italic ((,gui (:slant italic)) (,cui (:foreground ,red))))))

(provide-theme 'now)
