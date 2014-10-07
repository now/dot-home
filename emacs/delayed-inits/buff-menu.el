(define-key Buffer-menu-mode-map (kbd "%") nil)
(define-key Buffer-menu-mode-map (kbd "% f") 'Buffer-menu-mark-by-file-name-regexp)
(define-key Buffer-menu-mode-map (kbd "% f") 'Buffer-menu-mark-by-file-name-regexp)
(define-key Buffer-menu-mode-map (kbd "% m") 'Buffer-menu-mark-by-mode-regexp)
(define-key Buffer-menu-mode-map (kbd "% n") 'Buffer-menu-mark-by-name-regexp)
(define-key Buffer-menu-mode-map (kbd "* /") 'Buffer-menu-mark-dired-buffers)
(define-key Buffer-menu-mode-map (kbd "* M") 'Buffer-menu-mark-by-mode)
(define-key Buffer-menu-mode-map (kbd "* c") 'Buffer-menu-change-marks)
(define-key Buffer-menu-mode-map (kbd "* e") 'Buffer-menu-mark-dissociated-buffers)
(define-key Buffer-menu-mode-map (kbd "* h") 'Buffer-menu-mark-help-buffers)
(define-key Buffer-menu-mode-map (kbd "* m") 'Buffer-menu-mark-modified-buffers)
(define-key Buffer-menu-mode-map (kbd "* r") 'Buffer-menu-mark-read-only-buffers)
(define-key Buffer-menu-mode-map (kbd "* s") 'Buffer-menu-mark-special-buffers)
(define-key Buffer-menu-mode-map (kbd "* u") 'Buffer-menu-mark-unsaved-buffers)
(define-key Buffer-menu-mode-map (kbd "* z") 'Buffer-menu-mark-compressed-file-buffers)
(define-key Buffer-menu-mode-map (kbd ".") 'Buffer-menu-mark-old-buffers)
(define-key Buffer-menu-mode-map (kbd "U") 'Buffer-menu-unmark-all)
(define-key Buffer-menu-mode-map (kbd "r") 'Buffer-menu-toggle-read-only)
(eval-after-load 'evil
  '(progn
     (evil-define-key 'motion Buffer-menu-mode-map
       "s" 'evil-forward-char
       "w" 'Buffer-menu-save)))
