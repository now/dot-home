(setq viper-mode t
      viper-mode-ex-style-editing nil
      viper-want-ctl-h-help t
      viper-inhibit-startup-message 't
      viper-expert-level '5)
(require 'viper)
(require 'vimpulse)

(define-key viper-vi-basic-map "ga" 'what-cursor-position)
(define-key viper-vi-basic-map "U" 'undo-tree-redo)
(define-key viper-vi-basic-map "\C-r" 'isearch-backward-regexp)

(define-key viper-vi-global-user-map "s" 'viper-forward-char)
(define-key viper-vi-global-user-map "l" 'viper-substitute)
(define-key viper-vi-global-user-map "S" 'viper-window-bottom)
(define-key viper-vi-global-user-map "L" 'viper-substitute-line)
(define-key viper-vi-global-user-map "K" 'woman)
(define-key viper-vi-global-user-map " " 'viper-scroll-screen)
(define-key viper-vi-global-user-map [backspace] 'viper-scroll-screen-back)
(define-key viper-vi-global-user-map "\C-?" 'viper-scroll-screen-back)
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

(define-key viper-vi-global-user-map ",n" 'next-error)
(define-key viper-vi-global-user-map ",p" 'previous-error)
(define-key viper-vi-global-user-map ",N" 'compilation-next-file)
(define-key viper-vi-global-user-map ",P" 'compilation-previous-file)

(global-set-key (kbd "C-x C-o") 'other-window)

(define-key viper-insert-global-user-map "\C-k" 'digraph-read)

(define-key viper-vi-global-user-map "`" 'smex)

(setq viper-vi-state-mode-list
      (append viper-vi-state-mode-list
              '(grep-mode)))

(save-excursion
  (set-buffer "*Messages*")
  (viper-change-state-to-vi))

(defun close-buffer-and-window-unless-last ()
  (interactive)
  (let* ((buffer (current-buffer))
         (window (get-buffer-window buffer))
         (next (next-window window)))
    (kill-buffer buffer)
    (when (and window
               (not (eq window next)))
      (delete-window window))))

(defvar viper-grep-mode-fixes 
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'close-buffer-and-window-unless-last)
    map))
(viper-modify-major-mode 'grep-mode 'vi-state viper-grep-mode-fixes)
