(deftheme now
  "Color theme by Nikolai Weibull.")

(let ((custom--inhibit-theme-enable nil) ; Work around broken custom-theme-set-faces/theme-enable/face-spec-recalc
      (class '((class color) (min-colors 89)))
      (gui '((type x w32 mac)))
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
      (off-white "#f6f6f6")
      (brown "#af5f00")
      (off-red "RosyBrown1")
      (light-orange "#ffaf5f")
      (lighter-yellow "#ffd700"))
  (custom-theme-set-faces
    'now
    `(button ((,class (:foreground ,blue))))
    `(compilation-column-number ((,class (:inherit compilation-line-number))))
    `(compilation-error ((,class (:foreground ,light-red))))
    `(compilation-info ((,class (:foreground ,light-blue))))
    `(compilation-line-number ((,class (:foreground ,brown))))
    `(compilation-warning ((,class (:foreground ,red))))
    `(cursor ((,class (:foreground ,off-white :background ,off-black))))
    `(custom-button ((,gui (:box (:line-width 1 :color "#656a6f") :background "#e9ebed" :foreground ,off-black))))
    `(custom-button-mouse ((,gui (:box (:line-width 1 :color "#478cba") :background "#e9ebed" :foreground ,off-black))))
    `(custom-button-pressed ((,gui (:box (:line-width 1 :color "#35576d") :background "#d4d6da" :foreground ,off-black))))
    `(default ((,gui (:foreground ,off-black :background ,off-white))))
    `(diff-added ((,class (:foreground ,green))))
    `(diff-context ((,class (nil))))
    `(diff-file-header ((,class (:foreground ,purple))))
    `(diff-function ((,class (:foreground ,blue))))
    `(diff-header ((,class (:foreground ,purple))))
    `(diff-hunk-header ((,class (:foreground ,purple))))
    ; diff-index
    `(diff-nonexistent ((,class (:foreground ,red))))
    `(diff-refine-change ((,class (:foreground ,blue))))
    `(diff-removed ((,class (:foreground ,light-red))))
    `(dired-directory ((,class (:foreground ,light-blue))))
    `(escape-glyph ((,class (:foreground ,light-blue))))
    `(flymake-errline ((,class (:foreground ,off-white :background ,light-red))))
    `(flymake-warnline ((,class (:foreground ,off-white :background ,off-red))))
    `(font-lock-builtin-face ((,class (:foreground ,purple))))
    `(font-lock-comment-face ((,class (:foreground ,green))))
    `(font-lock-constant-face ((,class (:foreground ,red))))
    `(font-lock-function-name-face ((,class (:foreground ,blue))))
    `(font-lock-keyword-face ((,class (:foreground ,mustard))))
    `(font-lock-preprocessor-face ((,class (:foreground ,light-magenta))))
    `(font-lock-string-face ((,class (:foreground ,red))))
    `(font-lock-type-face ((,class (:foreground ,purple))))
    `(font-lock-variable-name-face ((,class (:inherit default))) t "Blah")
    `(font-lock-warning-face ((,class (:foreground ,off-white :background ,red :weight normal))))
    `(fringe ((,class (:background "grey95"))))
    `(header-line ((,class (:inherit mode-line))))
    `(highlight ((,class (:foreground ,off-white :background ,blue))))
    `(isearch ((,class (:foreground ,off-black :background ,lighter-yellow))))
    `(isearch-fail ((,class (:foreground ,off-white :background ,light-red))))
    `(italic ((,class (nil))))
    `(lazy-highlight ((,class (:foreground ,off-black :background ,light-yellow))))
    `(minibuffer-prompt ((,class (nil))))
    `(match ((,class (:inherit isearch))))
    `(mode-line ((,class (:box nil :background "grey70"))))
    `(mode-line-highlight ((,class (:box nil :underline t))))
    `(mode-line-inactive ((,class (:box nil :foreground "grey19" :background "grey86"))))
    `(region ((,class (:background ,light-cyan))))
    `(show-paren-match ((,class (:background ,light-orange))))
    `(show-paren-mismatch ((,class (:foreground ,off-white :background ,light-red))))
    `(ido-first-match ((,class (:inherit isearch))))
    `(ido-only-match ((,class (:foreground ,light-green))))
    `(ido-subdir ((,class (:inherit dired-directory))))
    `(link ((,class (:foreground ,blue))))
    `(link-visited ((,class (:foreground ,purple))))
    `(magit-diff-add ((,class (:inherit diff-added))))
    `(magit-diff-del ((,class (:inherit diff-removed))))
    `(magit-diff-hunk-header ((,class (:inherit diff-hunk-header))))
    `(magit-item-highlight ((,class (:inherit region))))
    ; magit-log-sha1
    ; magit-log-graph
    `(magit-section-title ((,class (:foreground ,light-blue))))
    `(ned-info-on-file-mode ((,class (:foreground ,green))))
    `(ned-info-on-file-read-only ((,class (:foreground ,red))))
    `(org-block ((,class (:foreground "grey40"))))
    `(org-code ((,class (:foreground "grey40"))))
    `(org-done ((,class (:foreground ,off-white :background ,green))))
    `(org-todo ((,class (:foreground ,off-white :background ,red))))
    `(secondary-selection ((,class (:background ,cyan))))
    `(trailing-whitespace ((,class (:forgeground ,off-white :background ,light-red))))
    `(underline ((,class (nil))))
    `(undo-tree-visualizer-active-branch-face ((t (nil))))
    `(whitespace-empty ((,class (:inherit whitespace-trailing))))
    `(whitespace-hspace ((,class (:inherit whitespace-space))))
    `(whitespace-space ((,class (:foreground ,light-blue))))
    `(whitespace-space-before-tab ((,class (:inherit whitespace-trailing))))
    `(whitespace-space-after-tab ((,class (:inherit whitespace-trailing))))
    `(whitespace-tab ((,class (:inherit whitespace-space))))
    `(whitespace-trailing ((,class (:inherit trailing-whitespace))))
    `(woman-bold ((,gui (:weight bold)) (,class (:foreground ,blue))))
    `(woman-italic ((,gui (:slant italic)) (,class (:foreground ,red))))))

(provide-theme 'now)
