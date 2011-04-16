(set-frame-font "DejaVu Sans Mono-9")
(setq default-frame-alist
      '((font . "DejaVu Sans Mono-9") (width . 98) (height . 70)))
;(setq default-buffer-file-coding-system
;      (coding-system-change-eol-conversion
;        default-buffer-file-coding-system
;        'unix))
;; TODO: set this for w32 only, if at all.
(setq keyboard-coding-system 'utf-8)
; (add-untranslated-filesystem (concat (user-login-name) "@" (system-name) ":"))
(require 'cygwin-mount)
(cygwin-mount-activate)
