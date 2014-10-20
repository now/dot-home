(eval-when-compile
  (require 'cl))

(setq-default evil-shift-width 2)
(setq evil-digraphs-table-user
      '(((?c ?b) . ?\x2022)
        ((?t ?b) . ?\x2023)
        ((?\( ?/) . ?\x2209)
        ((?. ?3) . ?\x2026)
        ((?, ?3) . ?\x22ef)
        ((?< ?Y) . ?\x227a)
        ((?< ?/) . ?\x27e8)
        ((?> ?/) . ?\x27e9)))

(delete 'shell-mode evil-insert-state-modes)

(defun evil-delete-auto-indent-on-insert-state-exit ()
  (if (and (eolp)
           (member last-command '(evil-ret
                                  evil-open-below
                                  evil-open-above
                                  reindent-then-newline-and-indent
                                  c-context-line-break
                                  c-electric-semi&comma)))
      (delete-horizontal-space)))
(add-hook 'evil-insert-state-exit-hook
          'evil-delete-auto-indent-on-insert-state-exit)

(cl-labels ((define-keys (map key def &rest bindings)
              (define-key map key def)
              (if bindings
                  (apply #'define-keys map bindings))))
  (define-keys evil-normal-state-map
    (kbd "DEL") 'evil-scroll-page-up
    "K" 'man
    "q" 'delete-other-windows
    "Q" 'evil-record-macro
    "U" 'undo-tree-redo
    "\M-d" 'smex
    "g\C-g" 'ned-info-on-file
    "gw" nil
    ",u" 'undo-tree-visualize
    ",w" 'save-buffer)
  (define-keys evil-visual-state-map
    "\C-d" 'evil-normal-state)
  (define-keys evil-insert-state-map
    "\C-d" 'evil-normal-state)
  (define-key evil-motion-state-map
    "," nil)
  (define-keys evil-motion-state-map
    " " 'evil-scroll-page-down
    "`" 'smex
    "~" 'smex-major-mode-commands
    "\C-d" 'suspend-frame
    ",a" 'org-agenda
    ",b" 'ido-switch-buffer
    ",B" 'buffer-menu
    ",c" 'project-shell-command
    ",C" 'shell-command
    ",d" 'dired
    ",e" 'find-file
    ",E" 'find-project-file
    ",k" 'ido-kill-buffer
    ",m" 'compile-project
    ",M" 'compile
    ",n" 'next-error
    ",N" 'compilation-next-file
    ",o" 'org-capture
    ",p" 'previous-error
    ",P" 'compilation-previous-file
    ",r" 'recompile
    ",s" 'magit-status
    ",W" 'save-some-buffers
    "gc" 'ace-jump-char-mode
    "gl" 'ace-jump-line-mode
    "gs" 'ace-jump-word-mode)
  (define-keys evil-read-key-map
    "\C-k" 'evil-insert-digraph
    "\C-^" 'evil-buffer)
  (define-keys evil-ex-search-keymap ; TODO This doesn’t work either.
    "\C-k" 'evil-insert-digraph)
  (define-keys minibuffer-local-map
    "\C-k" 'evil-insert-digraph))

(define-key evil-normal-state-map "s" nil)
(define-key evil-motion-state-map "l" nil)
(define-key evil-normal-state-map "l" 'evil-substitute)
(define-key evil-motion-state-map "s" 'evil-forward-char)

(define-key evil-normal-state-map "S" nil)
(define-key evil-motion-state-map "L" nil)
(define-key evil-normal-state-map "L" 'evil-change-whole-line)
(define-key evil-motion-state-map "S" 'evil-window-bottom)