(deftheme now
  "Color theme by Nikolai Weibull.")

(let ((class '((class color) (min-colors 89)))
      (gui '((type graphic)))
      (black "#000000")
      (red "#951616")
      (green "#257325")
      (blue "#2f5a9b")
      (mustard "#766020")
      (purple "#602f80")
      (cyan "#5694a8")
      (light-blue "#2080c0")
      (off-black "#181818")
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
      (yellow "#ffbd00"))
  (custom-theme-set-faces
    'now
    `(buffer-menu-buffer ((,class)))
    `(compilation-column-number ((,class (:inherit compilation-line-number))))
    `(compilation-line-number ((,class)))
    `(compilation-mode-line-exit ((,class (:inherit compilation-info))))
    `(compilation-mode-line-fail ((,class (:inherit compilation-error))))
    `(cursor ((,class (:foreground ,white :background ,black))))
    `(custom-button ((,gui (:box (:line-width 1 :color "#656a6f") :background "#e9ebed" :foreground ,black))))
    `(custom-button-mouse ((,gui (:box (:line-width 1 :color "#478cba") :background "#e9ebed" :foreground ,black))))
    `(custom-button-pressed ((,gui (:box (:line-width 1 :color "#35576d") :background "#d4d6da" :foreground ,black))))
    `(default ((,gui (:foreground ,black :background ,white :family "DejaVu Sans Mono" :height 140))))
    `(diff-added ((,class (:foreground ,green))))
    `(diff-context ((,class)))
    `(diff-file-header ((,class)))
    `(diff-header ((,class)))
    `(diff-nonexistent ((,class (:foreground ,red))))
    `(diff-refine-added ((,class (:inherit (diff-added diff-refine-change)))))
    `(diff-refine-removed ((,class (:inherit (diff-removed diff-refine-change)))))
    `(diff-removed ((,class (:foreground ,red))))
    `(dired-directory ((,class (:foreground ,blue))))
    `(dired-header ((,class (:inherit header))))
    `(dired-perm-write ((,class (:foreground "#ff00ff"))))
    `(dired-symlink ((,class (:foreground ,light-cyan))))
    `(error ((,class (:foreground ,white :background ,red))))
    `(escape-glyph ((,class (:foreground ,light-blue))))
    `(flyspell-duplicate ((,class (:foreground ,yellow :underline t))))
    `(flyspell-incorrect ((,class (:foreground ,red :underline t))))
    `(font-lock-builtin-face ((,class)))
    `(font-lock-comment-face ((,class (:background "grey90"))))
    `(font-lock-constant-face ((,class)))
    `(font-lock-doc-face ((,class (:inherit font-lock-comment-face))))
    `(font-lock-function-name-face ((,class)))
    `(font-lock-keyword-face ((,class)))
    `(font-lock-preprocessor-face ((,class)))
    `(font-lock-regexp-grouping-construct ((,class)))
    `(font-lock-string-face ((,class)))
    `(font-lock-type-face ((,class)))
    `(font-lock-variable-name-face ((,class)))
    `(font-lock-warning-face ((,class (:inherit warning))))
    `(git-commit-branch-face ((,class (:inherit font-lock-comment-face))))
    `(git-commit-comment-heading-face ((,class)))
    `(git-commit-comment-file-face ((,class)))
    `(glyphless-char ((,class (:inherit escape-glyph))))
    `(header-line ((,class (:inherit mode-line))))
    `(highlight ((,class (:foreground ,white :background ,blue))))
    `(holiday ((,class (:foreground ,white :background ,red))))
    `(ido-first-match ((,class (:inherit isearch))))
    `(ido-only-match ((,class (:inherit success))))
    `(ido-subdir ((,class (:inherit dired-directory))))
    `(ido-virtual ((,class (:foreground "grey50"))))
    `(info-menu-header ((,class)))
    `(info-menu-star ((,class)))
    `(info-title-1 ((,class)))
    `(info-title-2 ((,class)))
    `(info-title-3 ((,class)))
    `(info-title-4 ((,class)))
    `(info-node ((,class)))
    `(isearch ((,class (:foreground ,black :background ,lighter-yellow))))
    `(isearch-fail ((,class (:inherit error))))
    `(lazy-highlight ((,class (:foreground ,black :background ,light-yellow))))
    `(link ((,class (:foreground ,blue))))
    `(link-visited ((,class (:foreground ,purple))))
    `(magit-branch ((,class (:inherit dired-directory))))
    `(magit-log-head-label-bisect-bad ((,class (:inherit error))))
    `(magit-log-head-label-bisect-good ((,class (:inherit success))))
    `(magit-log-head-label-default ((,class (:background "grey90"))))
    `(magit-log-head-label-local ((,class (:foreground ,light-blue :background "grey90"))))
    `(magit-log-head-label-remote ((,class (:foreground ,green :background "grey90"))))
    `(magit-log-head-label-tags ((,class (:foreground ,brown :background "grey90"))))
    `(magit-log-sha1 ((,class)))
    `(makefile-space ((,class (:inherit error))))
    `(match ((,class (:inherit isearch))))
    `(minibuffer-prompt ((,class)))
    `(mode-line ((,class (:background "grey70"))))
    `(mode-line-buffer-id ((,class)))
    `(mode-line-highlight ((,class (:inherit link))))
    `(mode-line-inactive ((,class (:foreground "grey19" :background "grey86"))))
    `(ned-info-on-file-mode ((,class (:foreground ,green))))
    `(ned-info-on-file-read-only ((,class (:foreground ,red))))
    `(org-agenda-date-weekend ((,class)))
    `(org-agenda-done ((,class)))
    `(org-agenda-restriction-lock ((,class)))
    `(org-agenda-structure ((,class (:weight bold))))
    `(org-block ((,class (:foreground "grey40"))))
    `(org-code ((,class (:foreground "grey40"))))
    `(org-column ((,class (:foreground ,black))))
    `(org-date ((,class (:inherit font-lock-comment-face))))
    `(org-date-selected ((,class (:inherit region))))
    `(org-delegated ((,class (:inherit org-waiting))))
    `(org-done ((,class (:foreground ,white :background ,green))))
    `(org-hold ((,class (:background "grey70"))))
    `(org-hide ((,class (:foreground ,white))))
    `(org-next ((,class (:foreground ,white :background ,brown))))
    `(org-priority-a ((,class (:weight bold))))
    `(org-priority-b ((,class)))
    `(org-priority-c ((,class (:foreground "grey40"))))
    `(org-scheduled-today ((,class)))
    `(org-table ((,class)))
    `(org-tag ((,class)))
    `(org-todo ((,class (:foreground ,white :background ,red))))
    `(org-waiting ((,class (:background ,yellow))))
    `(outline-1 ((,class)))
    `(outline-2 ((,class)))
    `(outline-3 ((,class)))
    `(outline-4 ((,class)))
    `(outline-5 ((,class)))
    `(outline-6 ((,class)))
    `(outline-7 ((,class)))
    `(outline-8 ((,class)))
    `(region ((,class (:background ,light-cyan))))
    `(secondary-selection ((,class (:background ,cyan))))
    `(sh-heredoc ((,class (:inherit font-lock-string-face))))
    `(sh-quoted-exec ((,class (:inherit font-lock-string-face))))
    `(show-paren-match ((,class (:inherit success))))
    `(show-paren-mismatch ((,class (:inherit error))))
    `(success ((,class (:foreground ,white :background ,green))))
    `(trailing-whitespace ((,class (:inherit error))))
    `(undo-tree-visualizer-active-branch-face ((t)))
    `(warning ((,class (:foreground ,white :background ,yellow))))
    `(week ((,class (:inherit font-lock-comment-face))))
    `(whitespace-empty ((,class (:inherit whitespace-trailing))))
    `(whitespace-hspace ((,class (:inherit whitespace-space))))
    `(whitespace-indentation ((,class (:inherit whitespace-space))))
    `(whitespace-line ((,class (:inherit warning))))
    `(whitespace-newline ((,class (:inherite whitespace-space))))
    `(whitespace-space ((,class (:foreground ,light-blue))))
    `(whitespace-space-after-tab ((,class (:inherit whitespace-trailing))))
    `(whitespace-space-before-tab ((,class (:inherit whitespace-trailing))))
    `(whitespace-tab ((,class (:inherit whitespace-space))))
    `(whitespace-trailing ((,class (:inherit trailing-whitespace))))
    `(woman-bold ((,gui (:weight bold)) (,class (:foreground ,blue))))
    `(woman-italic ((,gui (:slant italic)) (,class (:foreground ,red))))))

(provide-theme 'now)
