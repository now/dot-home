(define-key Buffer-menu-mode-map "%" nil)
(define-key Buffer-menu-mode-map "%f" 'Buffer-menu-mark-by-file-name-regexp)
(define-key Buffer-menu-mode-map "%f" 'Buffer-menu-mark-by-file-name-regexp)
(define-key Buffer-menu-mode-map "%m" 'Buffer-menu-mark-by-mode-regexp)
(define-key Buffer-menu-mode-map "%n" 'Buffer-menu-mark-by-name-regexp)
(define-key Buffer-menu-mode-map "*/" 'Buffer-menu-mark-dired-buffers)
(define-key Buffer-menu-mode-map "*M" 'Buffer-menu-mark-by-mode)
(define-key Buffer-menu-mode-map "*c" 'Buffer-menu-change-marks)
(define-key Buffer-menu-mode-map "*e" 'Buffer-menu-mark-dissociated-buffers)
(define-key Buffer-menu-mode-map "*h" 'Buffer-menu-mark-help-buffers)
(define-key Buffer-menu-mode-map "*m" 'Buffer-menu-mark-modified-buffers)
(define-key Buffer-menu-mode-map "*r" 'Buffer-menu-mark-read-only-buffers)
(define-key Buffer-menu-mode-map "*s" 'Buffer-menu-mark-special-buffers)
(define-key Buffer-menu-mode-map "*u" 'Buffer-menu-mark-unsaved-buffers)
(define-key Buffer-menu-mode-map "*z" 'Buffer-menu-mark-compressed-file-buffers)
(define-key Buffer-menu-mode-map "." 'Buffer-menu-mark-old-buffers)
(define-key Buffer-menu-mode-map "Q" 'Buffer-menu-do-query-replace-regexp)
(define-key Buffer-menu-mode-map "U" 'Buffer-menu-unmark-all)
(define-key Buffer-menu-mode-map "r" 'Buffer-menu-toggle-read-only)
(eval-after-load 'evil
  '(progn
     (evil-define-key 'motion Buffer-menu-mode-map
       "s" 'evil-forward-char
       "w" 'Buffer-menu-save)))
