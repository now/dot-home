(eval-after-load 'evil-digraphs
  '(setq evil-digraphs-table-user
         '(((?c ?b) . ?\x2022)
           ((?t ?b) . ?\x2023)
           ((?\( ?/) . ?\x2209)
           ((?. ?3) . ?\x2026)
           ((?, ?3) . ?\x22ef)
           ((?< ?Y) . ?\x227a)
           ((?< ?/) . ?\x27e8)
           ((?> ?/) . ?\x27e9))))

(defun call-interactively-at-git-root (command &optional record-flag keys)
  "Call COMMAND interactively with DEFAULT-DIRECTORY set to directory containing `.git'."
  (let ((root (locate-dominating-file (or (buffer-file-name) default-directory) ".git")))
    (if root
        (let ((default-directory root))
          (call-interactively command record-flag keys))
      (call-interactively command record-flag keys))))

(defun find-vc-project-file ()
  "Find a file, starting at the vc project root."
  (interactive)
  (call-interactively-at-git-root 'find-file))

(defun vc-project-shell-command ()
  "Run SHELL-COMMAND with DEFAULT-DIRECTORY set to VC-GIT-ROOT."
  (interactive)
  (call-interactively-at-git-root 'shell-command))

(eval-after-load 'evil-vars
  '(delete 'shell-mode evil-insert-state-modes))

(eval-after-load 'evil-maps
  '(progn
     (define-key evil-normal-state-map "q" 'delete-other-windows)
     (define-key evil-normal-state-map "Q" 'evil-record-macro)

     (define-key evil-normal-state-map "s" nil)
     (define-key evil-motion-state-map "l" nil)
     (define-key evil-normal-state-map "l" 'evil-substitute)
     (define-key evil-motion-state-map "s" 'evil-forward-char)

     (define-key evil-normal-state-map "S" nil)
     (define-key evil-motion-state-map "L" nil)
     (define-key evil-normal-state-map "L" 'evil-change-whole-line)
     (define-key evil-motion-state-map "S" 'evil-window-bottom)

     (define-key evil-normal-state-map "K" 'man)

     (define-key evil-motion-state-map " " 'evil-scroll-page-down)
     (define-key evil-normal-state-map (kbd "DEL") 'evil-scroll-page-up)

     (define-key evil-normal-state-map ",b" 'ido-switch-buffer)
     (define-key evil-normal-state-map ",B" 'bs-show)
     (define-key evil-normal-state-map ",d" 'dired)
     (define-key evil-normal-state-map ",k" 'ido-kill-buffer)
     (define-key evil-normal-state-map ",w" 'save-buffer)
     (define-key evil-normal-state-map ",W" 'save-some-buffers)
     (define-key evil-normal-state-map "U" 'undo-tree-redo)
     (define-key evil-normal-state-map ",u" 'undo-tree-visualize)
     (define-key evil-normal-state-map "\C-d" 'suspend-frame)
     (define-key evil-normal-state-map "g\C-g" 'ned-info-on-file)
     (define-key evil-normal-state-map ",e" 'find-file)
     (define-key evil-normal-state-map ",E" 'find-vc-project-file)

     (define-key evil-normal-state-map ",c" 'vc-project-shell-command)
     (define-key evil-normal-state-map ",C" 'shell-command)

     (define-key evil-normal-state-map ",o" 'org-capture)

     (define-key evil-normal-state-map ",m" 'compile-package-immediately)
     (define-key evil-normal-state-map ",n" 'next-error)
     (define-key evil-normal-state-map ",p" 'previous-error)
     (define-key evil-normal-state-map ",N" 'compilation-next-file)
     (define-key evil-normal-state-map ",P" 'compilation-previous-file)

     (define-key evil-insert-state-map "\C-d" 'evil-normal-state)
     (define-key evil-insert-state-map "\C-^" 'evil-buffer)

     (global-set-key (kbd "C-x C-o") 'other-window)

     (define-key evil-normal-state-map "`" 'smex)
     (define-key evil-motion-state-map "`" 'smex)))

(defun evil-delete-auto-indent-on-insert-state-exit ()
  (if (and (eolp)
           (member last-command '(evil-ret
                                  evil-open-below
                                  evil-open-above
                                  reindent-then-newline-and-indent)))
      (delete-horizontal-space)))

(eval-after-load 'evil-insert
  '(add-hook 'evil-insert-state-exit-hook 'evil-delete-auto-indent-on-insert-state-exit))

(evil-mode 1)

;; TODO: This might not be needed anymore.
;;(define-key undo-tree-visualizer-map "s" 'undo-tree-visualize-switch-branch-right)
