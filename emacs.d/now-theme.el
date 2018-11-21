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
    `(avy-lead-face ((,class (:foreground ,black :background ,off-red))))
    `(avy-lead-face-0 ((,class (:inherit avy-lead-face))))
    `(avy-lead-face-1 ((,class (:inherit avy-lead-face))))
    `(avy-lead-face-2 ((,class (:inherit avy-lead-face))))
    `(buffer-menu-buffer ((,class)))
    `(compilation-column-number ((,class (:inherit compilation-line-number))))
    `(compilation-line-number ((,class)))
    `(compilation-mode-line-exit ((,class (:inherit compilation-info))))
    `(compilation-mode-line-fail ((,class (:inherit compilation-error))))
    `(cursor ((,class (:foreground ,white :background ,black))))
    `(custom-button ((,gui (:box (:line-width 1 :color "#656a6f")
                            :background "#e9ebed" :foreground ,black))))
    `(custom-button-mouse ((,gui (:box (:line-width 1 :color "#478cba")
                                  :background "#e9ebed" :foreground ,black))))
    `(custom-button-pressed ((,gui (:box (:line-width 1 :color "#35576d")
                                   :background "#d4d6da" :foreground ,black))))
    `(default ((,gui (:foreground ,black :background ,white
                      :family "DejaVu Sans Mono" :height 140))))
    `(diff-added ((,class (:foreground ,green))))
    `(diff-context ((,class)))
    `(diff-file-header ((,class)))
    `(diff-header ((,class)))
    `(diff-nonexistent ((,class (:foreground ,red))))
    `(diff-refine-added ((,class (:inherit (diff-added diff-refine-change)))))
    `(diff-refine-removed ((,class (:inherit
                                    (diff-removed diff-refine-change)))))
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
    `(font-lock-comment-face ((,class (:foreground ,green))))
    `(font-lock-constant-face ((,class)))
    `(font-lock-doc-face ((,class (:inherit font-lock-comment-face))))
    `(font-lock-function-name-face ((,class)))
    `(font-lock-keyword-face ((,class)))
    `(font-lock-preprocessor-face ((,class)))
    `(font-lock-regexp-grouping-backslash ((,class)))
    `(font-lock-regexp-grouping-construct ((,class)))
    `(font-lock-string-face ((,class)))
    `(font-lock-type-face ((,class)))
    `(font-lock-variable-name-face ((,class)))
    `(font-lock-warning-face ((,class (:inherit warning))))
    `(git-commit-branch-face ((,class (:inherit font-lock-comment-face))))
    `(git-commit-comment-heading-face ((,class)))
    `(git-commit-comment-file-face ((,class)))
    `(glyphless-char ((,class (:inherit escape-glyph))))
    `(gnus-summary-normal-ancient ((,class)))
    `(gnus-summary-normal-read ((,class)))
    `(gnus-summary-normal-ticked ((,class)))
    `(gnus-summary-normal-unread ((,class (:weight bold))))
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
    `(ivy-current-match ((,class (:inherit highlight))))
    `(ivy-cursor ((,class (:inherit cursor))))
    `(ivy-highlight-face ((,class (:foreground ,blue))))
    `(ivy-match-required-face ((,class (:inherit error))))
    `(ivy-minibuffer-match-face-1 ((,class (:inherit isearch))))
    `(ivy-minibuffer-match-face-2 ((,class (:inherit isearch))))
    `(ivy-minibuffer-match-face-3 ((,class (:inherit isearch))))
    `(ivy-minibuffer-match-face-4 ((,class (:inherit isearch))))
    `(ivy-remote ((,class)))
    `(ivy-virtual ((,class (:foreground "grey50"))))
    `(lazy-highlight ((,class (:foreground ,black :background ,light-yellow))))
    `(link ((,class (:foreground ,blue))))
    `(link-visited ((,class (:foreground ,purple))))
    `(magit-branch ((,class (:inherit dired-directory))))
    `(magit-diff-context-highlight ((,class (:inherit magit-diff-context
                                             :background "grey90"))))
    `(magit-log-head-label-bisect-bad ((,class (:inherit error))))
    `(magit-log-head-label-bisect-good ((,class (:inherit success))))
    `(magit-log-head-label-default ((,class (:background "grey90"))))
    `(magit-log-head-label-local ((,class (:foreground ,light-blue
                                           :background "grey90"))))
    `(magit-log-head-label-remote ((,class (:foreground ,green
                                            :background "grey90"))))
    `(magit-log-head-label-tags ((,class (:foreground ,brown
                                          :background "grey90"))))
    `(magit-log-sha1 ((,class)))
    `(magit-process-ng ((,class (:inherit error))))
    `(magit-process-ok ((,class)))
    `(magit-section-heading ((,class (:weight bold))))
    `(magit-section-highlight ((,class (:inherit highlight))))
    `(makefile-space ((,class (:inherit error))))
    `(match ((,class (:inherit isearch))))
    `(message-cited-text ((,class (:inherit font-lock-comment-face))))
    `(message-header-cc ((,class)))
    `(message-header-name ((,class)))
    `(message-header-other ((,class)))
    `(message-header-subject ((,class)))
    `(message-header-to ((,class)))
    `(minibuffer-prompt ((,class)))
    `(mode-line ((,class (:background "grey20" :foreground "grey60"))))
    `(mode-line-buffer-id ((,class)))
    `(mode-line-highlight ((,class (:inherit link))))
    `(mode-line-inactive ((,class (:foreground "grey20" :background "grey86"))))
    `(mu4e-compose-separator-face ((,class)))
    `(mu4e-flagged-face ((,class)))
    `(mu4e-header-highlight-face ((,class (:inherit highlight))))
    `(mu4e-header-key-face ((,class)))
    `(mu4e-trashed-face ((,class)))
    `(mu4e-unread-face ((,class)))
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
    `(org-done ((,class (:foreground ,white :background "#14892c"))))
    `(org-hold ((,class (:background "grey70"))))
    `(org-hide ((,class (:foreground ,white))))
    `(org-mode-line-clock ((,class)))
    `(org-mode-line-clock-overrun ((,class (:inherit warning))))
    `(org-next ((,class (:foreground "#403000" :background "#ffd351"))))
    `(org-priority-a ((,class (:weight bold))))
    `(org-priority-b ((,class)))
    `(org-priority-c ((,class (:foreground "grey40"))))
    `(org-scheduled-today ((,class)))
    `(org-table ((,class)))
    `(org-tag ((,class)))
    `(org-todo ((,class (:foreground ,white :background "#4a6785"))))
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
    `(undo-tree-visualizer-active-branch-face ((t)))
    `(undo-tree-visualizer-current-face ((t)))
    `(undo-tree-visualizer-unmodified-face ((,class (:foreground ,green))))
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
    `(whitespace-trailing ((,class (:inherit error))))
    `(woman-bold ((,gui (:weight bold)) (,class (:foreground ,blue))))
    `(woman-italic ((,gui (:slant italic)) (,class (:foreground ,red))))))

(provide-theme 'now)
