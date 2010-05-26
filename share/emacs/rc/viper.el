(setq viper-mode t
      viper-mode-ex-style-editing nil
      viper-want-ctl-h-help t
      viper-inhibit-startup-message 't
      viper-expert-level '5)
(require 'viper)
(require 'vimpulse)

(define-key viper-vi-basic-map "ga" 'what-cursor-position)

(define-key viper-vi-global-user-map "s" 'viper-forward-char)
(define-key viper-vi-global-user-map "l" 'viper-substitute)
(define-key viper-vi-global-user-map "S" 'viper-window-bottom)
(define-key viper-vi-global-user-map "L" 'viper-substitute-line)
(define-key viper-vi-global-user-map "K" 'woman)
(define-key viper-vi-global-user-map " " 'viper-scroll-screen)
(define-key viper-vi-global-user-map [backspace] 'viper-scroll-screen-back)
(define-key viper-vi-global-user-map "\C-n" 'bs-cycle-next)
(define-key viper-vi-global-user-map "\C-p" 'bs-cycle-previous)

(define-key viper-insert-global-user-map "\C-m" 'newline-and-indent)
(define-key viper-insert-global-user-map "\C-o" 'viper-toggle-key-action)
(define-key viper-insert-global-user-map "\C-z" 'viper-intercept-ESC-key)
(define-key viper-vi-global-user-map ",e" 'find-file)
(defun find-vc-project-file ()
  (interactive)
  ;; TODO: Use actual vc-interface for this.  There should be a function to get
  ;; the root of a file under vc.
  (if vc-mode
    (let ((default-directory (vc-git-root (buffer-file-name))))
      (call-interactively 'find-file))
  (call-interactively 'find-file)))
(define-key viper-vi-global-user-map ",E" 'find-vc-project-file)

(define-key viper-insert-global-user-map "\C-k" 'digraph-read)
