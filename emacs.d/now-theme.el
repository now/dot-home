(deftheme now
  "Color theme by Nikolai Weibull.")

(let* ((cui '((class color) (min-colors 89)))
       (gtk '((type gtk)))
       (gui '((type graphic)))
       (dark '((background dark)))
       (lite '((background light)))
       (cui-dark `(,@cui ,@dark))
       (gtk-dark `(,@gtk ,@dark))
       (gui-dark `(,@gui ,@dark))
       (cui-lite `(,@cui ,@lite))
       (gtk-lite `(,@gtk ,@lite))
       (gui-lite `(,@gui ,@lite))
       (black "#000000")
       (red "#951616")
       (green "#257325")
       (blue "#2f5a9b")
       (mustard "#766020")
       (purple "#602f80")
       (cyan "#5694a8")
       (light-blue "#2080c0")
       (off-black "#1e1e1e")
       (off-white "#dedede")
       (light-red "#f02626")
       (light-green "#009000")
       (light-yellow "#f0a500")
       (light-magenta "#933763")
       (light-cyan "#80b0c0")
       (brown "#af5f00")
       (off-red "RosyBrown1")
       (light-orange "#ffaf5f")
       (lighter-yellow "#ffd700")
       (white "#ffffff")
       (yellow "#ffbd00")
       ;; macOS
       (selection-dark "#1c5ac8")
       ;; macOS
       (selection-lite "#2165d9"))
  (custom-theme-set-variables
   'now
   '(face-font-family-alternatives
     '(("Monospace" "DejaVu Sans Mono" "fixed")
       ("Monospace Serif" "DejaVu Sans Mono" "fixed")
       ("Sans Serif" "DejaVu Sans" "fixed")))
   '(magit-diff-highlight-hunk-region-functions
     '(magit-diff-highlight-hunk-region-dim-outside
       magit-diff-highlight-hunk-region-using-face)))
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
    `(default ((default . (:family "DejaVu Sans Mono" :height 120))
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
    `(error ((,cui (:foreground ,white :background ,red))))
    `(escape-glyph ((,cui (:foreground ,light-blue))))
    `(flymake-warning ((,cui (:underline (:style wave :color ,yellow)))))
    `(flyspell-duplicate ((,cui (:foreground ,yellow :underline t))))
    `(flyspell-incorrect ((,cui (:foreground ,red :underline t))))
    `(font-lock-builtin-face ((,cui)))
    `(font-lock-comment-face ((,cui (:foreground ,green))))
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
    `(markdown-code-face ((,cui (:foreground "grey40"))))
    `(markdown-list-face ((,cui)))
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
    `(secondary-selection ((,cui (:background ,cyan))))
    `(sh-heredoc ((,cui (:inherit font-lock-string-face))))
    `(sh-quoted-exec ((,cui (:inherit font-lock-string-face))))
    `(show-paren-match ((,cui (:inherit success))))
    `(show-paren-mismatch ((,cui (:inherit error))))
    `(success ((,cui (:foreground ,white :background ,green))))
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
    `(whitespace-trailing ((,cui (:inherit error))))
    `(woman-bold ((,gui (:weight bold)) (,cui (:foreground ,blue))))
    `(woman-italic ((,gui (:slant italic)) (,cui (:foreground ,red))))))

(provide-theme 'now)
