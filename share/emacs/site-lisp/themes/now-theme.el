(deftheme now
  "Color theme by Nikolai Weibull.")

(let ((class '((class color) (min-colors 89)))
      (blank '(:foreground unspecified :background unspecified :weight unspecified :box unspecified :inherit unspecified))
      (gui '((type x w32 mac)))
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
      (off-white "#f6f6f6")
      (brown "#af5f00")
      (off-red "RosyBrown1")
      (light-orange "#ffaf5f")
      (lighter-yellow "#ffd700")
      (yellow "#ffbd00"))
  (custom-theme-set-faces
    'now
    `(compilation-column-number ((,class (:inherit compilation-line-number))))
    `(compilation-line-number ((,class ,blank)))
    `(cursor ((,class (:foreground ,off-white :background ,black))))
    `(custom-button ((,gui (:box (:line-width 1 :color "#656a6f") :background "#e9ebed" :foreground ,black))))
    `(custom-button-mouse ((,gui (:box (:line-width 1 :color "#478cba") :background "#e9ebed" :foreground ,black))))
    `(custom-button-pressed ((,gui (:box (:line-width 1 :color "#35576d") :background "#d4d6da" :foreground ,black))))
    `(default ((,gui (:foreground ,black :background ,off-white))))
    `(diff-added ((,class (:foreground ,green))))
    `(diff-context ((,class ,blank)))
    `(diff-file-header ((,class ,blank)))
    `(diff-header ((,class ,blank)))
    `(diff-nonexistent ((,class (:foreground ,red))))
    `(diff-refine-added ((,class (:inherit (diff-added diff-refine-change)))))
    `(diff-refine-removed ((,class (:inherit (diff-removed diff-refine-change)))))
    `(diff-removed ((,class (:foreground ,red))))
    `(dired-directory ((,class (:foreground ,light-blue))))
    `(error ((,class (,@blank :foreground ,off-white :background ,red))))
    `(escape-glyph ((,class (:foreground ,light-blue))))
    `(font-lock-builtin-face ((,class ,blank)))
    `(font-lock-comment-face ((,class (:foreground ,green))))
    `(font-lock-constant-face ((,class ,blank)))
    `(font-lock-function-name-face ((,class ,blank)))
    `(font-lock-keyword-face ((,class ,blank)))
    `(font-lock-preprocessor-face ((,class ,blank)))
    `(font-lock-string-face ((,class (:foreground ,red))))
    `(font-lock-type-face ((,class ,blank)))
    `(font-lock-variable-name-face ((,class ,blank)))
    `(font-lock-warning-face ((,class (:inherit warning))))
    `(glyphless-char ((,class (:inherit escape-glyph))))
    `(header-line ((,class (:inherit mode-line))))
    `(highlight ((,class (:foreground ,off-white :background ,blue))))
    `(ido-first-match ((,class (:weight unspecified :inherit isearch))))
    `(ido-only-match ((,class (:foreground unspecified :inherit success))))
    `(ido-subdir ((,class (:foreground unspecified :inherit dired-directory))))
    `(ido-virtual ((,class ,blank)))
    `(isearch ((,class (:foreground ,black :background ,lighter-yellow))))
    `(isearch-fail ((,class (,@blank :inherit error))))
    `(lazy-highlight ((,class (:foreground ,black :background ,light-yellow))))
    `(link ((,class (:foreground ,blue))))
    `(link-visited ((,class (:foreground ,purple))))
    `(magit-branch ((,class (:inherit dired-directory))))
    `(magit-log-head-label-bisect-bad ((,class (,@blank :inherit error))))
    `(magit-log-head-label-bisect-good ((,class (,@blank :inherit success))))
    ; magit-log-head-label-*
    `(magit-log-sha1 ((,class ,blank)))
    `(match ((,class (:inherit isearch))))
    `(minibuffer-prompt ((,class ,blank)))
    `(mode-line ((,class (,@blank :background "grey70"))))
    `(mode-line-buffer-id ((,class ,blank)))
    `(mode-line-highlight ((,class (,@blank :inherit link))))
    `(mode-line-inactive ((,class (,@blank :foreground "grey19" :background "grey86"))))
    `(ned-info-on-file-mode ((,class (:foreground ,green))))
    `(ned-info-on-file-read-only ((,class (:foreground ,red))))
    `(org-block ((,class (:foreground "grey40"))))
    `(org-code ((,class (:foreground "grey40"))))
    `(org-done ((,class (:foreground ,off-white :background ,green))))
    `(org-todo ((,class (:foreground ,off-white :background ,red))))
    `(outline-1 ((,class ,blank)))
    `(outline-2 ((,class ,blank)))
    `(outline-3 ((,class ,blank)))
    `(outline-4 ((,class ,blank)))
    `(outline-5 ((,class ,blank)))
    `(outline-6 ((,class ,blank)))
    `(outline-7 ((,class ,blank)))
    `(outline-8 ((,class ,blank)))
    `(region ((,class (:background ,light-cyan))))
    `(secondary-selection ((,class (:background ,cyan))))
    `(sh-heredoc ((,class (:inherit font-lock-string-face))))
    `(show-paren-match ((,class (,@blank :inherit success))))
    `(show-paren-mismatch ((,class (,@blank :inherit error))))
    `(success ((,class (,@blank :foreground ,off-white :background ,green))))
    `(trailing-whitespace ((,class (,@blank :inherit error))))
    `(undo-tree-visualizer-active-branch-face ((t ,blank)))
    `(warning ((,class (,@blank :foreground ,off-white :background ,yellow))))
    `(whitespace-empty ((,class (,@blank :inherit whitespace-trailing))))
    `(whitespace-hspace ((,class (,@blank :inherit whitespace-space))))
    `(whitespace-indentation ((,class (,@blank :inherit whitespace-space))))
    `(whitespace-line ((,class (,@blank :inherit warning))))
    `(whitespace-newline ((,class (,@blank :inherite whitespace-space))))
    `(whitespace-space ((,class (,@blank :foreground ,light-blue))))
    `(whitespace-space-after-tab ((,class (,@blank :inherit whitespace-trailing))))
    `(whitespace-space-before-tab ((,class (,@blank :inherit whitespace-trailing))))
    `(whitespace-tab ((,class (,@blank :inherit whitespace-space))))
    `(whitespace-trailing ((,class (,@blank :inherit trailing-whitespace))))
    `(woman-bold ((,gui (:weight bold)) (,class (:foreground ,blue))))
    `(woman-italic ((,gui (:slant italic)) (,class (:foreground ,red))))))

(provide-theme 'now)
