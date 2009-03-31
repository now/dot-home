;; -*- mode: emacs-lisp; coding: utf-8 -*-

(require 'cl)

(labels ((build-path (&rest components)
                     (let ((directories (mapcar #'file-name-as-directory (butlast components)))
                           (file (car (last components))))
                       (concat (apply #'concat directories) file))))
  (let ((my-share-emacs-path (build-path (expand-file-name "~") "share" "emacs")))
    (add-to-list 'load-path my-share-emacs-path)
    (labels ((add (path) (add-to-list 'load-path (build-path my-share-emacs-path path)))
             (rc (missing-ok &rest components) (load (apply #'build-path my-share-emacs-path "rc" components) missing-ok))
             (rc-progmode (mode) (rc nil "progmodes" mode)))
      (add "ned")
      (add "icicles")
      (rc t "os" (symbol-name window-system))
      ;; TODO: Rename ruby-mode.
      (rc-progmode "ruby"))))

(require 'whitespace)

;; Turn off unnecessary UI clutter.
;; Add 'menu-bar-mode sooner or later.
(dolist (ui (list 'scroll-bar-mode 'tool-bar-mode))
  (if (fboundp ui)
    (funcall ui -1)))

;; TODO: This seems unimportant to set.
;(setq inhibit-startup-message t)
;; TODO: Is it really worth the trouble?
(setq inhibit-startup-echo-area-message "now")
(setq inhibit-startup-echo-area-message "nweibull")
(setq initial-scratch-message nil)

(fset 'yes-or-no-p 'y-or-n-p)
;(setq ring-bell-function 'ignore)

;(windmove-default-keybindings)

(blink-cursor-mode -1)

(setq-default indicate-buffer-boundaries '((bottom . left)))

;; Show matching parentheses.
(show-paren-mode 1)
(setq show-paren-delay 0)

(setq-default show-trailing-whitespace t)

;; Indentation.
(setq-default indent-tabs-mode nil)

;;;(require 'rnc-mode)

;;;;;(autoload 'rnc-mode "rnc-mode.el")
;;;(setq auto-mode-alist       
;;;      (cons '("\\.rnc\\'" . rnc-mode) auto-mode-alist))

(setq viper-mode t
      viper-mode-ex-style-editing nil
      viper-want-ctl-h-help t
      viper-inhibit-startup-message 't
      viper-expert-level '5)
(require 'viper)

(setq woman-use-own-frame nil
      woman-bold-headings nil)
; TODO: This is a bit overzealous, as we only want this under certain
; conditions.  Check the evernote about a way to set this dynamically for one
; keybinding.
;(setq woman-use-topic-at-point t)

(require 'rect-mark)

(require 'vimpulse)

; TODO: Undo/Redo for window configurations
; (winner-mode t)

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

; TODO: Figure out undo (to be like it is in Vim).  Look at undo-browse.el and
; is there a way to make sure that <backspace><backspace> is combined into one
; undo?

;; Might need to move these to c hook.
(setq-default fill-column 79)
(auto-fill-mode 1)

; TODO: Bind TAB to simply indent?
; TODO: It seems like Viper is getting in the way of CC mode for hungry
; submode.
; (global-set-key "\\C-m" 'newline-and-indent)
(defun my-make-CR-do-indent ()
  (define-key c-mode-base-map "\C-m" 'c-context-line-break))
(add-hook 'c-initialization-hook 'my-make-CR-do-indent)
(add-hook 'c-mode-common-hook
          (lambda ()
            (c-toggle-hungry-state 1)
            (c-toggle-auto-newline 1)))

; TODO: Add (add-to-list 'c-cleanup-list 'defun-close-semi)?
; We don’t need it right now.

(defconst now-c-style
  '("linux"
    (c-hanging-braces-alist . ((brace-list-intro . ())
                               (brace-list-close . ())
                               (brace-entry-open . ())
                               (class-close . ())))
    (c-hanging-colons-alist . ((case-label . (after))
                               (label . (after)))))
  "now’s C Programming Style")
(c-add-style "now-c-style" now-c-style)
(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "now-c-style")))
;; TODO: This doesn’t work.
(define-key viper-vi-global-user-map ",f" (text-properties-at (point)))

;; enable-recursive-minibuffers

;; Look at what-cursor-position for suggested implementation for ga (g8 might
;; be harder)
;; TODO: Best way to do this is to call what-cursor-position with positive
;; argument (C-u C-x =).

;; TODO: This is probably uninteresting as we should implement Vim’s Ctrl-V
;; instead.
(setq read-quoted-char-radix 16)

(require 'hide-mode-line)
(hide-mode-line)

(setq compilation-auto-jump-to-first-error t
      compilation-scroll-output t)

(add-hook
  'after-save-hook
  (lambda ()
    (when (string-match-p "/dot-home/emacs$" (buffer-file-name))
      (compile "make -k install")
      (load-file "~/.emacs"))))

(setq compilation-finish-functions 'compilation-finish-autoclose)
(defun compilation-finish-autoclose (buffer string)
  (when (and (not compilation-current-error) (not (string-match-p "exited abnormally" string)))
    (bury-buffer)
    ;; TODO: We should really be able to tell how many windows there are opened
    ;; before we begin compiling.  If there are two, this should be
    ;; replace-buffer-in-windows instead.
    (delete-window (get-buffer-window buffer t))))

(require 'color-theme)
(color-theme-initialize)
(color-theme-now)

(grep-compute-defaults)
(setq grep-command "grep -nH -P -e ")

(setq history-length 512
      make-backup-files nil)

;; TODO: Set faces for diff mode.
;; TODO: write-region-inhibit-fsync t?

(desktop-save-mode 1)
(dolist (variable '(command-history
                    read-expression-history
                    viper-quote-region-history
                    viper-search-history))
  (add-to-list 'desktop-globals-to-save variable))

(lexical-let ((global-desktop-dirname (expand-file-name "~/.cache/emacs")))
  (setq desktop-path (list "." global-desktop-dirname))
  (defun desktop-save-globally ()
    (interactive)
    (setq desktop-dirname global-desktop-dirname))
  (defun desktop-save-locally ()
    (interactive)
    (setq desktop-dirname (expand-file-name "."))))
(desktop-save-globally)

(setq digraph-table-user
  '((?\( ?/ ?\x2209)
    (?. ?3 ?\x2026)
    (?, ?3 ?\x22ef)
    (?< ?Y ?\x227a)
    (?< ?/ ?\x27e8)
    (?> ?/ ?\x27e9)))

(require 'digraph)
(define-key viper-insert-global-user-map "\C-k" 'digraph-read)

(require 'ned-info-on-file)

;(require 'anything)
;(require 'anything-config)
;(add-to-list 'anything-sources 'anything-c-source-files-in-current-dir)
(require 'ido)
(ido-mode 1)
(ido-everywhere 1)
(setq ido-enable-flex-matching t
      ido-use-filename-at-point t)
;; ido-enter-matching-directory t?
; TODO: What does this do?
;(setq ido-auto-merge-work-directories-length -1)
(add-hook 'ido-setup-hook
          (lambda ()
            (define-key ido-completion-map "\C-p" 'ido-prev-match)
            (define-key ido-completion-map "\C-n" 'ido-next-match)
            (define-key ido-completion-map [remap backward-delete-char-untabify] 'ido-delete-backward-updir)))
(icomplete-mode 1)
(setq completion-show-help nil)
;; TODO: Don’t know about this one.
(defun my-completion-delete-prompt ()
  (set-buffer standard-output)
  (goto-char (point-min))
  (delete-region (point) (search-forward "Possible completions are:\n")))
(add-hook 'completion-setup-hook 'my-completion-delete-prompt 'append)
;; TODO: Look into partial-completion-mode.
(partial-completion-mode 1)

(setq eol-mnemonic-unix ""
      eol-mnemonic-mac "mac"
      eol-mnemonic-dos "dos"
      eol-mnemonic-undecided "?")

(setq-default mode-line-remote '(:eval (if (file-remote-p default-directory) "@" ""))
              mode-line-buffer-identification (propertized-buffer-identification "%b")
              mode-line-frame-identification '(:eval (if (or (null window-system) (eq window-system 'pc)) "[%F] " ""))
              mode-line-modes (butlast mode-line-modes)
              ; TODO: 'mode-line-remote might be overkill.  Is it really
              ; pertinent information?
              mode-line-format (list
                                 ""
                                 'mode-line-client
                                 'mode-line-remote
                                 'mode-line-frame-identification
                                 'mode-line-buffer-identification
                                 '(:propertize " " 'help-echo help-echo)
                                 'mode-line-modes
                                 '(vc-mode vc-mode)))

; (setq isearch-resume-in-command-history t)
; (setq search-ring-max 100 regexp-search-ring-max 100)
(define-key global-map "\C-s" 'isearch-forward-regexp)
(define-key esc-map "\C-s" 'isearch-forward)
(define-key global-map "\C-r" 'isearch-backward-regexp)
(define-key esc-map "\C-r" 'isearch-backward)

;; Clear echo area.
;; TODO: This doesn’t clear messages from desktop, so it’s more or less
;; useless.
(message "")

;set completeopt=menu,menuone,preview
;
;noremap <Leader>p :cprevious<CR>
;noremap <Leader>n :cnext<CR>
;noremap <Leader>P :cpfile<CR>
;noremap <Leader>N :cnfile<CR>
;
;noremap <silent> <Leader>s <Esc>:setlocal invspell spelllang=en_us<CR>
;
;nnoremap <silent> ,k :bn <Bar> :bd #<CR>
;
;cnoremap <C-Y> <C-R><C-O>*
;
;       dired-listing-switches "-l"      ; Don't display dot files 
;       dired-recursive-deletes 'top     ; Recursive deletes 
;       dired-recursive-copies 'top      ; Recursive copies 
;  
; ;;__________________________________________________________________________ 
; ;;;;    Programming - Common Lisp
;  
; ;; Specify modes for Lisp file extensions 
; (setq auto-mode-alist 
;       (append '( 
; 		("\.lisp$" . lisp-mode) 
; 		("\.lsp$" . lisp-mode) 
; 		("\.cl$" . lisp-mode) 
; 		("\.asd$" . lisp-mode) 
; 		) auto-mode-alist))
;  
; (require 'slime-autoloads)
;  
; (eval-after-load "slime" 
;   '(progn 
;      (setq inferior-lisp-program "sbcl" 
; 	   slime-complete-symbol*-fancy t 
;            slime-complete-symbol-function 'slime-fuzzy-complete-symbol 
;            slime-when-complete-filename-expand t 
;            slime-truncate-lines nil 
;            slime-autodoc-use-multiline-p t)
;  
;      (slime-setup '(slime-fancy slime-asdf))
;  
;      (define-key slime-repl-mode-map (kbd "C-c ;") 'slime-insert-balanced-comments) 
;      (define-key slime-repl-mode-map (kbd "C-c M-;") 'slime-remove-balanced-comments) 
;      (define-key slime-mode-map (kbd "C-c ;") 'slime-insert-balanced-comments) 
;      (define-key slime-mode-map (kbd "C-c M-;") 'slime-remove-balanced-comments) 
;      (define-key slime-mode-map (kbd "RET") 'newline-and-indent) 
;      (define-key slime-mode-map (kbd "") 'newline-and-indent) 
;      (define-key slime-mode-map (kbd "C-j") 'newline)))
;  
; (add-hook 'lisp-mode-hook (lambda () 
;                             (cond ((not (featurep 'slime)) 
;                                    (require 'slime) 
;                                    (normal-mode))) 
; 			    (modify-syntax-entry ?- "w")))
;  
; ;;__________________________________________________________________________ 
; ;;;;    Programming - Elisp
;  
; (add-hook 'emacs-lisp-mode-hook 
; 	  '(lambda () 
; 	     (interactive) 
; 	     (define-key emacs-lisp-mode-map (kbd "RET") 'newline-and-indent) 
; 	     (define-key emacs-lisp-mode-map (kbd "") 'newline-and-indent) 
; 	     (define-key emacs-lisp-mode-map (kbd "C-j") 'newline) 
; 	     (require 'eldoc) 
; 	     (turn-on-eldoc-mode) 
; 	     (modify-syntax-entry ?- "w"))) ; now '-' is not considered a word-delimiter
