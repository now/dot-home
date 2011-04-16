(deftheme now
  "Color theme by Nikolai Weibull.")

(let ((class '((class color) (min-colors 89)))
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
      (light-yellow "#f0a500")
      (light-magenta "#933763")
      (light-cyan "#80b0c0")
      (off-white "#f6f6f6")
      (brown "#af5f00")
      (off-red "RosyBrown1")
      (light-orange "#ffaf5f"))
  (custom-theme-set-faces
    'now
    `(default ((,gui (:foreground ,off-black :background ,off-white))))
    `(cursor ((,class (:foreground ,off-white :background ,off-black))))
    `(fringe ((,class (:background "grey95"))))
    `(mode-line ((,class (:box nil :background "grey70"))))
    `(mode-line-highlight ((,class (:box nil :underline t))))
    `(mode-line-inactive ((,class (:box nil :foreground "grey19" :background "grey86"))))
    `(minibuffer-prompt ((,class (nil))))
    `(escape-glyph ((,class (:foreground ,light-blue))))
    `(highlight ((,class (:background ,blue))))
    `(button ((,class (:underline t :foreground ,blue))))
    `(link ((,class (:underline t :foreground ,blue))))
    `(visited-link ((,class (:underline t :foreground ,purple))))
    `(lazy-highlight ((,class (:foreground ,off-black :background ,light-yellow))))
    `(trailing-whitespace ((,class (:background ,light-red))))
    `(region ((,class (:background ,light-cyan))))
    `(secondary-selection ((,class (:background ,cyan))))
    `(isearch ((,class (:foreground ,off-white :background ,brown))))
    `(isearch-fail ((,class (:foreground ,off-white :background ,light-red))))
    `(compilation-error ((,class (:foreground ,light-red))))
    `(diff-added ((,class (:foreground ,blue))))
    `(diff-context ((,class (nil))))
    `(diff-file-header ((,class (:foreground ,purple))))
    `(diff-function ((,class (:foreground ,blue))))
    `(diff-header ((,class (:foreground ,purple))))
    `(diff-hunk-header ((,class (:foreground ,purple))))
    `(diff-nonexistent ((,class (:foreground ,red))))
    `(diff-refine-change ((,class (:foreground ,blue))))
    `(diff-removed ((,class (:foreground ,light-red))))
    ; diff-index
;hi  DiffAdd       ctermfg=White       ctermbg=DarkGreen                   guifg=White       guibg=#257325
;hi  DiffChange    ctermfg=White       ctermbg=DarkBlue                    guifg=White       guibg=#2f5a9b
;hi  DiffDelete    ctermfg=White       ctermbg=DarkRed                     guifg=White       guibg=#951616   gui=None
;hi  DiffText      ctermfg=Red         ctermbg=None                        guifg=#f02626     guibg=NONE

    `(flymake-errline ((,class (:foreground ,off-white :background ,light-red))))
    `(flymake-warnline ((,class (:foreground ,off-white :background ,off-red))))
    `(show-paren-match ((,class (:background ,light-orange))))
    `(show-paren-mismatch ((,class (:foreground ,off-white :background ,red))))
    `(ido-first-match ((,class (:foreground ,off-white :background ,brown))))
    `(ido-subdir ((,class (:foreground ,light-blue))))
    `(undo-tree-visualizer-active-branch-face ((t (nil))))
    `(viper-minibuffer-emacs ((t (nil))))
    `(viper-minibuffer-insert ((t (nil))))
    `(viper-minibuffer-vi ((t (nil))))
    `(ned-info-on-file-mode ((,class (:foreground ,green))))
    `(ned-info-on-file-read-only ((,class (:foreground ,red))))
    `(custom-button ((,gui (:box (:line-width 1 :color "#656a6f") :background "#e9ebed" :foreground ,off-black))))
    `(custom-button-mouse ((,gui (:box (:line-width 1 :color "#478cba") :background "#e9ebed" :foreground ,off-black))))
    `(custom-button-pressed ((,gui (:box (:line-width 1 :color "#35576d") :background "#d4d6da" :foreground ,off-black))))
    `(woman-italic ((,gui (:slant italic)) (,class (:foreground ,red))))
    `(woman-bold ((,gui (:weight bold)) (,class (:foreground ,blue))))
    `(font-lock-builtin-face ((,class (:foreground ,purple))))
    `(font-lock-comment-face ((,class (:foreground ,green))))
    `(font-lock-constant-face ((,class (:foreground ,red))))
    `(font-lock-function-name-face ((,class (:foreground ,blue))))
    `(font-lock-keyword-face ((,class (:foreground ,mustard))))
    `(font-lock-preprocessor-face ((,class (:foreground ,light-magenta))))
    `(font-lock-string-face ((,class (:foreground ,red))))
    `(font-lock-type-face ((,class (:foreground ,purple))))
    `(font-lock-variable-name-face ((,class (:foreground ,blue))))
    `(font-lock-warning-face ((,class (:foreground ,off-white :background ,red :weight normal))))))

(provide-theme 'now)

;hi  WarningMsg                                                            guifg=#951616
;hi  Folded        ctermfg=None        ctermbg=254                         guifg=NONE        guibg=#e4e4e4
;hi! link          FoldColumn          Folded
;hi  SignColumn                                                            guifg=#2f5a9b     guibg=#a8a8a8
;hi  SpellBad                          ctermbg=224
;hi  SpellCap                                                                                                                guisp=#5fd7ff
;hi  SpellRare                         ctermbg=253                                                                           guisp=#dadada
;hi  SpellLocal                                                                                                              guisp=#80b0b0
;hi  Error                                                                                   guibg=#f02626
