(eval-when-compile
  (require 'cl))

(require 'evil-multiedit)

(setq-default evil-shift-width 2
              evil-symbol-word-search t
              evil-want-abbrev-expand-on-insert-exit nil)
(remove-hook 'evil-insert-state-exit-hook 'expand-abbrev)
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

(dolist (m '(archive-mode git-commit-mode git-rebase-mode tar-mode))
  (setq evil-emacs-state-modes (delete m evil-emacs-state-modes)))
(setq evil-insert-state-modes (delete 'term-mode evil-insert-state-modes))
(add-to-list 'evil-emacs-state-modes 'term-mode)

(cl-labels ((define-keys (map key def &rest bindings)
              (define-key map key def)
              (if bindings
                  (apply #'define-keys map bindings))))
  (define-keys evil-normal-state-map
    (kbd "DEL") 'evil-scroll-page-up
    (kbd "S-SPC") 'evil-scroll-page-up
    "K" 'man
    "q" 'delete-other-windows
    "Q" 'evil-record-macro
    "U" 'undo-tree-redo
    "\C-r" nil
    "\M-d" 'smex
    "\M-n" 'evil-multiedit-match-symbol-and-next
    "\M-p" 'evil-multiedit-match-symbol-and-prev
    "\C-t" 'xref-pop-marker-stack
    "g\C-g" 'hide-mode-line-unhide-temporarily
    "gw" nil
    "zC" 'hs-hide-all-comments
    ",e" 'find-file
    ",E" 'now-gxref-find-file
    ",l" 'now-occur
    ",u" 'undo-tree-visualize
    ",w" 'save-buffer)
  (define-keys evil-visual-state-map
    "R" 'evil-multiedit-match-all
    "\M-n" 'evil-multiedit-match-and-next
    "\M-p" 'evil-multiedit-match-and-prev
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
    "gb" 'backward-sexp
    "gw" 'forward-sexp
    "\C-d" 'evil-insert
    ",a" 'org-agenda
    ",b" 'ido-switch-buffer
    ",B" 'buffer-menu
    ",c" 'shell-command
    ",d" 'dired
    ",k" 'ido-kill-buffer
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
    "gs" 'evil-ace-jump-word-mode
    (kbd "RET") 'evil-multiedit-toggle-or-restrict-region)
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

(with-eval-after-load 'arc-mode
  (defvar archive-mode-map)
  (evil-make-overriding-map archive-mode-map 'normal)
  (evil-define-key 'motion archive-mode-map
    "j" 'archive-next-line
    "k" 'archive-previous-line
    "h" 'evil-backward-char))

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

(evil-define-key 'normal view-mode-map
  "q" 'quit-window)

(evil-define-key 'normal xref--xref-buffer-mode-map
  "q" 'quit-window)

(with-eval-after-load 'mu4e
  (defvar mu4e-main-mode-map)
  ;; Keys to remember : S, $
  ;; Obvious keys here: b, B, s, q, j, C, m, f, U, ;
  (evil-make-overriding-map mu4e-main-mode-map 'normal)
  (defvar mu4e-headers-mode-map)
  (evil-make-overriding-map mu4e-headers-mode-map 'normal)
  ;; Keys to remember : s, S, /, b, B, O, Q, W, %, [, ], ;, y, ?, !, A,
  ;;                    +, -, =, &, #, $, H
  ;; Obvious keys here: q, g, t, T, p, n, d, m, r, u, *, U, x, a,
  ;;                    R, F, C, E, RET
  ;; TODO Add o or v as alternative to y?
  (evil-define-key 'normal mu4e-headers-mode-map
    "G" 'mu4e-update-mail-and-index
    "J" 'mu4e-headers-jump-to-maildir
    "\C-i" 'mu4e-headers-query-next
    "\C-o" 'mu4e-headers-query-prev
    "j" 'mu4e-headers-next
    "k" 'mu4e-headers-prev
    "l" 'mu4e-headers-mark-for-later
    "q" 'now-mu4e-headers-quit-buffer
    (kbd "<backspace>") 'scroll-down-command)
  (defvar mu4e-view-mode-map)
  (evil-make-overriding-map mu4e-view-mode-map 'normal)
  ;; Keys to remember : TAB, C, c, s, S, /, b, B, %, v, f, |, ;, O,
  ;;                    Q, W, y, e, o, A, d, m, r, ?, !, +, -, =,
  ;;                    &, *, #, w, h, #, M-q, $
  ;; Obvious keys here: q, t, T, g, F, R, C, E, ., a, p, n, [, ],
  ;;                    u, U, x
  (evil-define-key 'normal mu4e-view-mode-map
    "#" 'mu4e-mark-resolve-deferred-marks
    "G" 'mu4e-update-mail-and-index
    "J" 'mu4e-headers-jump-to-maildir
    "K" 'mu4e-view-save-url
    "\C-i" 'mu4e-headers-query-next
    "\C-o" 'mu4e-headers-query-prev
    "l" 'mu4e-view-mark-for-later
    "zr" 'mu4e-view-toggle-hide-cited
    (kbd "S-SPC") 'now-mu4e-view-scroll-down-or-prev)
  (evil-set-initial-state 'mu4e-main-mode 'normal)
  (evil-set-initial-state 'mu4e-headers-mode 'normal)
  (evil-set-initial-state 'mu4e-view-mode 'normal)
  (evil-set-initial-state 'mu4e~update-mail-mode 'normal))

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

(defun now-occur (&optional arg)
  "Limit display to lines containing current symbol.
A prefix ARG specifies how many lines of context to keep."
  (interactive "P")
  (setq iedit-unmatched-lines-invisible nil)
  (if (evil-multiedit-state-p)
      (progn
        (evil-multiedit-abort))
    (evil-multiedit-match-all)
    (iedit-show/hide-unmatched-lines arg)
    (recenter)))
