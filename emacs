(require 'cl)

(let ((my-share-emacs-path (concat (expand-file-name "~") "/.local/share/emacs")))
  (push my-share-emacs-path load-path)
  (labels ((add (path)
                (push (concat my-share-emacs-path "/" path) load-path)))))

(require 'ruby-mode)
(require 'flymake)

(global-font-lock-mode 1)

;; Turn off unnecessary UI clutter.
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
;
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message (user-login-name))

(blink-cursor-mode -1)
(show-paren-mode 1)
(setq show-paren-delay 0)
(set-face-background 'show-paren-match-face "#ffaf5f")
(set-face-foreground 'show-paren-match-face (face-foreground 'default))

(set-default-font "DejaVu Sans Mono-9")

;; I don't like the default colors :)
;;
(set-face-background 'flymake-errline "red4")
(set-face-background 'flymake-warnline "dark slate blue")

;; Invoke ruby with '-c' to get syntax checking
(defun flymake-ruby-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "ruby" (list "-c" local-file))))

(push '(".+\\.rb$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("Rakefile$" flymake-ruby-init) flymake-allowed-file-name-masks)

(push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3) flymake-err-line-patterns)

(add-hook 'ruby-mode-hook
          '(lambda ()

             ;; Don't want flymake mode for ruby regions in rhtml files and also on read only files
             (if (and (not (null buffer-file-name)) (file-writable-p buffer-file-name))
                 (flymake-mode))
             ))

;;;(require 'rnc-mode)

;;;;;(autoload 'rnc-mode "rnc-mode.el")
;;;(setq auto-mode-alist       
;;;      (cons '("\\.rnc\\'" . rnc-mode) auto-mode-alist))

(setq viper-mode t)
(setq viper-mode-ex-style-editing nil)
(setq viper-inhibit-startup-message 't)
(setq viper-expert-level '1)
(require 'viper)

(setq woman-use-own-frame nil)
(setq woman-use-topic-at-point t)

(require 'rect-mark)

;;; Start XO

;;__________________________________________________________________________ 
;;;;    System Customizations 
 
; (defconst use-backup-dir t)    
; (setq backup-directory-alist (quote ((".*" . "~/.backups/"))) 
;       version-control t		       ; Use version numbers for backups 
;       kept-new-versions 16	       ; Number of newest versions to keep 
;       kept-old-versions 2	       ; Number of oldest versions to keep 
;       delete-old-versions t            ; Delete excess backup versions 
;       history-delete-duplicates t      ; Delete dups in history 
;       history-length 100               ; Larger history size than default 30 
;       inhibit-splash-screen t          ; No initial splash screen 
;       dired-listing-switches "-l"      ; Don't display dot files 
;       dired-recursive-deletes 'top     ; Recursive deletes 
;       dired-recursive-copies 'top      ; Recursive copies 
;       backup-by-copying-when-linked t) ; Copy linked files, don't rename
;  
; (fset 'yes-or-no-p 'y-or-n-p)          ;replace y-e-s by y
;  
; ;; Conventional mouse/arrow movement & selection 
; (pc-selection-mode)                  
; (delete-selection-mode t)
;  
; ;; Display overrides 
; (show-paren-mode 1) 
; (tool-bar-mode nil) 
; (menu-bar-mode nil) 
; (iswitchb-mode)
;  
; ;; Global key overrides 
; (global-set-key [(control c) (F)] 'ffap) 
; (global-set-key [(control c) (j)] 'join-line)
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
;  
; ;;__________________________________________________________________________ 
; ;;;;    Initial buffer
;  
; (find-file "~/")
; 
; ;;; End XO
