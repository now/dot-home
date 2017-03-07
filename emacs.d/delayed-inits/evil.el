(eval-when-compile
  (require 'cl))

(setq-default evil-shift-width 2
              evil-symbol-word-search t)
(setq evil-digraphs-table-user
      '(((?c ?b) . ?\x2022)
        ((?t ?b) . ?\x2023)
        ((?\( ?/) . ?\x2209)
        ((?. ?3) . ?\x2026)
        ((?, ?3) . ?\x22ef)
        ((?< ?Y) . ?\x227a)
        ((?< ?/) . ?\x27e8)
        ((?> ?/) . ?\x27e9))
      evil-move-cursor-back nil)

(setq evil-insert-state-modes (delete 'term-mode evil-insert-state-modes))
(add-to-list 'evil-emacs-state-modes 'term-mode)

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
    "\C-r" nil
    "\M-d" 'smex
    "\C-t" 'xref-pop-marker-stack
    "g\C-g" 'hide-mode-line-unhide-temporarily
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
    "\C-]" 'xref-find-definitions
    "g\C-]" 'xref-find-references
    "\C-d" 'suspend-frame
    ",a" 'org-agenda
    ",b" 'ido-switch-buffer
    ",B" 'buffer-menu
    ",c" 'shell-command
    ",d" 'dired
    ",e" 'find-file
    ",k" 'ido-kill-buffer
    ",l" 'loccur-current
    ",L" 'loccur
    ",m" 'compile
    ",n" 'next-error
    ",N" 'compilation-next-file
    ",O" 'org-capture
    ",p" 'previous-error
    ",P" 'compilation-previous-file
    ",r" 'recompile
    ",s" 'magit-status
    ",W" 'save-some-buffers
    "gc" 'evil-ace-jump-char-mode
    "gl" 'evil-ace-jump-line-mode
    "gs" 'evil-ace-jump-word-mode)
  (define-keys evil-read-key-map
    "\C-k" 'evil-insert-digraph)
  (define-keys evil-replace-state-map
    "\C-d" 'evil-normal-state))

(define-key evil-normal-state-map "s" nil)
(define-key evil-motion-state-map "l" nil)
(define-key evil-normal-state-map "l" 'evil-substitute)
(define-key evil-motion-state-map "s" 'evil-forward-char)

(define-key evil-normal-state-map "S" nil)
(define-key evil-motion-state-map "L" nil)
(define-key evil-normal-state-map "L" 'evil-change-whole-line)
(define-key evil-motion-state-map "S" 'evil-window-bottom)

;;; Mode-specific bindings

(evil-define-key 'motion Buffer-menu-mode-map
  "s" 'evil-forward-char
  "w" 'Buffer-menu-save)

(evil-define-key 'motion calendar-mode-map
  "\C-b" 'calendar-scroll-right-three-months
  "\C-f" 'calendar-scroll-left-three-months
  "H" 'calendar-cursor-holidays
  "b" 'calendar-beginning-of-week
  "h" 'calendar-backward-day
  "j" 'calendar-forward-week
  "k" 'calendar-backward-week
  "s" 'calendar-forward-day
  "w" 'calendar-end-of-week)

(defvar epa-key-list-mode-map)
(with-eval-after-load 'epa
  (evil-make-overriding-map epa-key-list-mode-map nil))

(defvar grep-mode-map)
(with-eval-after-load 'grep
  (evil-make-overriding-map grep-mode-map nil))

(evil-define-key 'motion Info-mode-map
  "\C-i" 'Info-history-forward)

(evil-define-key 'normal Man-mode-map
  "q" 'Man-quit)

(evil-define-key 'motion org-mode-map
  (kbd "RET") 'org-cycle)
(evil-define-key 'normal org-mode-map
  ",<" 'org-mobile-pull
  ",>" 'org-mobile-push
  ",i" 'org-clock-in
  ",o" 'org-clock-out
  ",P" 'org-set-property
  ",T" 'org-set-effort
  ",t" 'org-todo)

(evil-define-key 'motion paredit-mode-map
  "gB" 'evil-paredit-backward-up
  "gb" 'evil-paredit-backward
  "gW" 'evil-paredit-forward-up
  "gw" 'evil-paredit-forward)
(evil-define-key 'normal paredit-mode-map
  "D" 'paredit-kill
  "x" 'paredit-forward-delete
  "X" 'paredit-backward-delete
  "))" 'evil-paredit-forward-slurp-sexp
  ")}" 'evil-paredit-forward-barf-sexp
  "((" 'evil-paredit-backward-slurp-sexp
  "({" 'evil-paredit-backward-barf-sexp
  "(J" 'paredit-join-sexps
  "(R" 'paredit-raise-sexp
  "(S" 'paredit-splice-sexp-killing-backward
  "(s" 'paredit-splice-sexp
  "(W" 'paredit-wrap-round)

(evil-define-key 'normal ruby-mode-map
  ",t" 'ruby-find-other-file
  ",M" 'ruby-run-test-at-line)

(defun evil-delete-auto-indent-on-insert-state-exit ()
  (when (eolp)
    (cond
     ((member last-command '(evil-ret
                              c-context-line-break
                              newline))
      (delete-horizontal-space))
     ((member last-command '(c-electric-brace
                             c-electric-colon
                             c-electric-semi&comma))
      (delete-horizontal-space)
      (when (bolp)
        (delete-char -1))))))
(add-hook 'evil-insert-state-exit-hook
          'evil-delete-auto-indent-on-insert-state-exit)

(evil-put-command-property 'xref-find-definitions :jump t)
