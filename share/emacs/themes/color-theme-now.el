(eval-when-compile
  (require 'color-theme))

; TODO: We can also use color-X as the name for color X.
(defun color-theme-now ()
  "Color theme by Nikolai Weibull."
  (interactive)
  (color-theme-install
      '(color-theme-now
         ((background-mode . light))
         (cursor (((min-colors 16777216) (class color))
                  (:foreground "#f6f6f6"
                   :background "#181818")))
         (default (((min-colors 16777216) (class color))
                   (:foreground "#181818"
                    :background "#f6f6f6")))
         (compilation-error ((((min-colors 16777216) (class color))
                              (:foreground "#f02626"))
                             (((min-colors 16) (class color))
                              (:foreground "red"))
                             (t
                               (:inverse-video t))))
         (custom-button ((((type x w32 mac) (class color))
                          (:box (:line-width 1 :color "#656a6f")
                           :background "#e9ebed"
                           :foreground "black"))
                         (t
                           nil)))
         (custom-button-mouse ((((type x w32 mac) (class color))
                                (:box (:line-width 1 :color "#478cba")
                                 :background "#e9ebed"
                                 :foreground "black"))
                               (t
                                 nil)))
         (custom-button-pressed ((((type x w32 mac) (class color))
                                  (:box (:line-width 1 :color "#35576d")
                                   :background "#d4d6da"
                                   :foreground "black"))
                                 (t
                                   nil)))
         (flymake-errline ((((min-colors 16777216) (class color))
                            (:foreground "#f6f6f6"
                             :background "#951616"))
                           (((min-colors 16) (class color))
                            (:foreground "white"
                             :background "dark red"))
                           (t
                             (:weight bold))))
         (flymake-warnline ((((min-colors 88) (class color))
                             (:foreground "#f6f6f6"
                              :background "RosyBrown1"))
                            (((min-colors 16) (class color))
                             (:foreground "white"
                              :background "dark cyan"))
                            (t
                              (:weight bold))))
         (font-lock-builtin-face ((((min-colors 16777216) (class color))
                                   (:foreground "#602f80"))
                                  (((min-colors 16) (class color))
                                   (:foreground "dark magenta"))
                                  (t
                                    (:underline t))))
         (font-lock-comment-face ((((min-colors 16777216) (class color))
                                   (:foreground "#257325"))
                                  (((min-colors 16) (class color))
                                   (:foreground "dark green"))
                                  (t
                                    (:weight bold))))
         (font-lock-constant-face ((((min-colors 16777216) (class color))
                                    (:foreground "#951616"))
                                   (((min-colors 16) (class color))
                                    (:foreground "dark red"))
                                   (t
                                     (:underline t))))
         (font-lock-function-name-face ((((min-colors 16777216) (class color))
                                         (:foreground "#2f5a9b"))
                                        (((min-colors 16) (class color))
                                         (:foreground "dark blue"))
                                        (t
                                          (:underline t))))
         (font-lock-keyword-face ((((min-colors 16777216) (class color))
                                   (:foreground "#766020"))
                                  (((min-colors 16) (class color))
                                   (:foreground "dark yellow"))
                                  (t
                                    (:weight bold))))
         (font-lock-preprocessor-face ((((min-colors 16777216) (class color))
                                        (:foreground "#93376e"))
                                       (((min-colors 16) (class color))
                                        (:foreground "magenta"))
                                       (t
                                         (:underline t))))
         (font-lock-string-face ((((min-colors 16777216) (class color))
                                  (:foreground "#951616"))
                                 (((min-colors 16) (class color))
                                  (:foreground "dark red"))
                                 (t
                                   (:underline t))))
         (font-lock-type-face ((((min-colors 16777216) (class color))
                                (:foreground "#602f80"))
                               (((min-colors 16) (class color))
                                (:foreground "dark magenta"))
                               (t
                                 (:underline t))))
         (font-lock-variable-name-face ((((min-colors 16777216) (class color))
                                         (:foreground "#2f5a9b"))
                                        (((min-colors 16) (class color))
                                         (:foreground "dark blue"))
                                        (t
                                          (:underline t))))
         (font-lock-warning-face ((((min-colors 16777216) (class color))
                                   (:foreground "white"
                                    :background "#951616"
                                    :weight normal))
                                  (((min-colors 16) (class color))
                                   (:foreground "white"
                                    :background "dark red"
                                    :weight normal))
                                  (t
                                    (:inverse-video t
                                     :weight normal))))
         (fringe ((((class color))
                   (:background "grey95"))))
         (isearch ((((min-colors 16777216) (class color))
                    (:foreground "white"
                     :background "#af5f00"))
                   (((min-colors 256) (class color))
                    (:foreground "white"
                     :background "DarkOrange3"))
                   (((min-colors 16) (class color))
                    (:foreground "white"
                     :background "dark yellow"))
                   (t
                     (:inverse-video t))))
         ; isearch-fail
         (minibuffer-prompt ((t (nil))))
         (mode-line ((t
                       (:box nil
                        :background "grey70"))))
         (mode-line-highlight ((t
                                (:box nil
                                 :underline t))))
         (mode-line-inactive ((t
                                (:box nil
                                 :foreground "grey19"
                                 :background "grey86"))))
         (ned-info-on-file-mode ((((min-colors 16777216) (class color))
                                  (:foreground "#257325"))
                                 (((min-colors 16) (class color))
                                  (:foreground "dark green"))
                                 (t
                                   (:inverse-video t))))
         (ned-info-on-file-read-only ((((min-colors 16777216) (class color))
                                       (:foreground "#951616"))
                                      (((min-colors 16) (class color))
                                       (:foreground "dark red"))
                                      (t
                                        (:inverse-video t))))
         (show-paren-match ((((min-colors 16777216) (class color))
                             (:background "#ffaf5f"))
                            (((min-colors 256) (class color))
                             (:background "tan1")) ; TODO: Check this
                            (((min-colors 16) (class color))
                             (:background "yellow"))
                            (t
                              (:inverse-video t))))
         (show-paren-mismatch ((((class color))
                                (:foreground "white"
                                 :background "red"))
                               (t
                                 (:inverse-video t))))
         (woman-italic ((((type x w32 mac))
                              (:slant italic))
                             (((class color))
                              (:foreground "red"))
                             (t
                              (:inverse-video t))))
         (woman-bold ((((type x w32 mac))
                            (:weight bold))
                           (((class color))
                            (:foreground "blue"))
                           (t
                            (:inverse-video t))))
         (ido-first-match ((((min-colors 16777216) (class color))
                            (:foreground "white"
                             :background "#af5f00"))
                           (((min-colors 256) (class color))
                            (:foreground "white"
                             :background "DarkOrange3"))
                           (((min-colors 16) (class color))
                            (:foreground "white"
                             :background "dark yellow"))
                           (t
                             (:inverse-video t))))
         (ido-subdir ((((min-colors 16777216) (class color))
                       (:foreground "#2080c0"))
                      (((min-colors 16) (class color))
                       (:foreground "blue"))))
         (viper-minibuffer-emacs ((t (nil))))
         (viper-minibuffer-insert ((t (nil))))
         (viper-minibuffer-vi ((t (nil))))
         )))

;hi  WarningMsg                                                            guifg=#951616
;hi  Folded        ctermfg=None        ctermbg=254                         guifg=NONE        guibg=#e4e4e4
;hi! link          FoldColumn          Folded
;hi  DiffAdd       ctermfg=White       ctermbg=DarkGreen                   guifg=White       guibg=#257325
;hi  DiffChange    ctermfg=White       ctermbg=DarkBlue                    guifg=White       guibg=#2f5a9b
;hi  DiffDelete    ctermfg=White       ctermbg=DarkRed                     guifg=White       guibg=#951616   gui=None
;hi  DiffText      ctermfg=Red         ctermbg=None                        guifg=#f02626     guibg=NONE
;hi  SignColumn                                                            guifg=#2f5a9b     guibg=#a8a8a8
;hi  SpellBad                          ctermbg=224
;hi  SpellCap                                                                                                                guisp=#5fd7ff
;hi  SpellRare                         ctermbg=253                                                                           guisp=#dadada
;hi  SpellLocal                                                                                                              guisp=#80b0b0
;hi  Error                                                                                   guibg=#f02626

(provide 'color-theme-now)
