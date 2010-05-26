;; -*- mode: emacs-lisp; coding: utf-8 -*-

(require 'cl)

(labels ((build-path (&rest components)
                     (let ((directories (mapcar #'file-name-as-directory (butlast components)))
                           (file (car (last components))))
                       (concat (apply #'concat directories) file))))
  (let ((my-share-emacs-path (build-path (expand-file-name "~") "share" "emacs")))
    (add-to-list 'load-path my-share-emacs-path)
    (labels ((add (path) (add-to-list 'load-path (build-path my-share-emacs-path path)))
             (load-rc (missing-ok &rest components)
                      (load (apply #'build-path my-share-emacs-path "rc" components) missing-ok))
             (rc (&rest components) (apply #'load-rc nil components))
             (rc-progmode (mode) (rc "progmodes" mode)))
      (add "ned")
      (add "icicles")
      (load-rc t "os" (symbol-name window-system))
      (rc-progmode "cc-mode")
      (rc-progmode "compile")
      (rc-progmode "grep")
      (rc-progmode "ruby-mode")
      (rc "ui")
      (rc "desktop")
      (rc "digraph")
      (rc "ido")
      (rc "icomplete")
      (rc "isearch")
      (rc "viper"))))

(require 'whitespace)

;(windmove-default-keybindings)

;; Indentation.
(setq-default indent-tabs-mode nil)

;;;(require 'rnc-mode)

;;;;;(autoload 'rnc-mode "rnc-mode.el")
;;;(setq auto-mode-alist       
;;;      (cons '("\\.rnc\\'" . rnc-mode) auto-mode-alist))

(setq woman-use-own-frame nil
      woman-bold-headings nil)
; TODO: This is a bit overzealous, as we only want this under certain
; conditions.  Check the evernote about a way to set this dynamically for one
; keybinding.
;(setq woman-use-topic-at-point t)

; TODO: Undo/Redo for window configurations
; (winner-mode t)

; TODO: Figure out undo (to be like it is in Vim).  Look at undo-browse.el and
; is there a way to make sure that <backspace><backspace> is combined into one
; undo?

;; Might need to move these to c hook.
(setq-default fill-column 79)
(auto-fill-mode 1)

;; enable-recursive-minibuffers

;; TODO: This is probably uninteresting as we should implement Vim’s Ctrl-V
;; instead.
(setq read-quoted-char-radix 16)

(add-hook
  'after-save-hook
  (lambda ()
    (when (string-match-p "/dot-home/emacs$" (buffer-file-name))
      (compile "make -k install")
      (load-file "~/.emacs"))))

(setq history-length 512
      make-backup-files nil)

;; TODO: write-region-inhibit-fsync t?

(require 'ned-info-on-file)

;(require 'anything)
;(require 'anything-config)
;(add-to-list 'anything-sources 'anything-c-source-files-in-current-dir)

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
