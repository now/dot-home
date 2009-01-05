(set-frame-font "DejaVu Sans Mono-9")
(setq initial-frame-alist
      '((width . 98) (height . 70)))
(setq default-buffer-file-coding-system
      (coding-system-change-eol-conversion
        default-buffer-file-coding-system
        'unix))
; (add-untranslated-filesystem (concat (user-login-name) "@" (system-name) ":"))
(require 'cygwin-mount)
(cygwin-mount-activate)
