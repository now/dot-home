(require 'evil)
(evil-mode 1)

(setq evil-digraphs-table-user
  '((?\( ?/ ?\x2209)
    (?. ?3 ?\x2026)
    (?, ?3 ?\x22ef)
    (?< ?Y ?\x227a)
    (?< ?/ ?\x27e8)
    (?> ?/ ?\x27e9)))

(define-key evil-normal-state-map "q" 'delete-other-windows)
(define-key evil-normal-state-map "Q" 'evil-record-macro)

(define-key evil-normal-state-map "s" 'undefined)
(define-key evil-normal-state-map "l" 'evil-substitute)
(define-key evil-motion-state-map "s" 'evil-forward-char)

(define-key evil-normal-state-map "L" 'evil-change-whole-line)
(define-key evil-motion-state-map "S" 'evil-window-bottom)

(define-key evil-normal-state-map "K" 'woman)

(define-key evil-motion-state-map " " 'evil-scroll-page-down)
(define-key evil-normal-state-map (kbd "DEL") 'evil-scroll-page-up)

;(define-key evil-normal-state-map "\C-n" 'bs-cycle-next)
;(define-key evil-normal-state-map "\C-p" 'bs-cycle-previous)
(define-key evil-normal-state-map ",b" 'ido-switch-buffer)
(define-key evil-normal-state-map ",k" 'ido-kill-buffer)
(define-key evil-normal-state-map "U" 'undo-tree-redo)
(define-key evil-normal-state-map "\C-d" 'suspend-frame)
(define-key evil-normal-state-map "g\C-g" 'ned-info-on-file)

(define-key evil-normal-state-map ",e" 'find-file)
(defun find-vc-project-file ()
  (interactive)
  (if vc-mode
    (let ((default-directory (vc-git-root (buffer-file-name))))
      (call-interactively 'find-file))
  (call-interactively 'find-file)))
(define-key evil-normal-state-map ",E" 'find-vc-project-file)

(define-key evil-normal-state-map ",n" 'next-error)
(define-key evil-normal-state-map ",p" 'previous-error)
(define-key evil-normal-state-map ",N" 'compilation-next-file)
(define-key evil-normal-state-map ",P" 'compilation-previous-file)

(define-key evil-insert-state-map "\C-d" 'evil-normal-state)

(global-set-key (kbd "C-x C-o") 'other-window)

(define-key evil-normal-state-map "`" 'smex)

(defun close-buffer-and-window-unless-last ()
  (interactive)
  (let* ((buffer (current-buffer))
         (window (get-buffer-window buffer))
         (next (next-window window)))
    (kill-buffer buffer)
    (when (and window
               (not (eq window next)))
      (delete-window window))))

(evil-declare-key 'normal grep-mode-map
  "q" 'close-buffer-and-window-unless-last)
(evil-declare-key 'normal diff-mode-map
  "q" 'close-buffer-and-window-unless-last)
