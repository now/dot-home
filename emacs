;; -*- mode: emacs-lisp; coding: utf-8 -*-

(require 'cl)

(let ((my-share-emacs-path (concat (expand-file-name "~") "/.local/share/emacs")))
  (push my-share-emacs-path load-path)
  (labels ((add (path)
                (push (concat my-share-emacs-path "/" path) load-path)))
    (add "ned")))

(require 'ruby-mode)
(require 'flymake)

;; Turn off unnecessary UI clutter.
;; Add 'menu-bar-mode sooner or later.
(dolist (ui (list 'scroll-bar-mode 'tool-bar-mode))
  (if (fboundp ui)
    (funcall ui -1)))

(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message "nweibull")
(setq inhibit-startup-echo-area-message "now")
(setq initial-scratch-message nil)

(fset 'yes-or-no-p 'y-or-n-p)

;; Set up rest of UI.
(global-font-lock-mode 1)
(set-frame-font "DejaVu Sans Mono-9")

(blink-cursor-mode -1)

(setq-default indicate-buffer-boundaries '((bottom . left)))

;; Show matching parentheses.
(show-paren-mode 1)
(setq show-paren-delay 0)

;; Indentation.
(setq-default indent-tabs-mode nil)
;(setq-default tab-width 8)

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

(if (eq window-system 'w32)
  (progn
;    (add-untranslated-filesystem (concat (user-login-name) "@" (system-name) ":"))
    (setq default-buffer-file-coding-system
          (coding-system-change-eol-conversion
            default-buffer-file-coding-system
            'unix))
    (setq initial-frame-alist
          `((width . 98) (height . 70)))))

;;;(require 'rnc-mode)

;;;;;(autoload 'rnc-mode "rnc-mode.el")
;;;(setq auto-mode-alist       
;;;      (cons '("\\.rnc\\'" . rnc-mode) auto-mode-alist))

(setq viper-mode t
      viper-mode-ex-style-editing nil
      viper-ESC-moves-cursor-back nil
      viper-want-ctl-h-help t)
(setq viper-inhibit-startup-message 't)
(setq viper-expert-level '5)
(require 'viper)

(setq woman-use-own-frame nil)
(setq woman-use-topic-at-point t)

(require 'rect-mark)

(require 'vimpulse)

(setq eol-mnemonic-unix ""
      eol-mnemonic-mac "mac"
      eol-mnemonic-dos "dos"
      eol-mnemonic-undecided "?")

(defun join-strings (strings &optional delim)
  (with-output-to-string
    (loop for (string . more?) on strings
          do (princ string)
          when more? (princ delim))))

(labels ((define-key-if (pair)
                        (let ((definition (lookup-key viper-vi-basic-map (cadr pair))))
                          (if definition (define-key viper-vi-global-user-map (car pair) definition))))
         (swap (pair)
               (define-key-if pair)
               (define-key-if (reverse pair)))
         (swap-case (pair)
                    (swap pair)
                    (swap (mapcar #'upcase pair))))
  (loop for pair on '("t" "j" "n" "k" "s" "l") by #'cddr
        do (swap-case (list (car pair) (cadr pair)))))
(define-key viper-vi-global-user-map "K" 'woman)
(define-key viper-vi-global-user-map " " 'viper-scroll-screen)
(define-key viper-vi-global-user-map [backspace] 'viper-scroll-screen-back)
(define-key viper-vi-global-user-map "\C-n" 'bs-cycle-next)
(define-key viper-vi-global-user-map "\C-p" 'bs-cycle-previous)

; TODO:
; 1.  Fix mode-line

(define-key viper-vi-global-user-map ",f" (text-properties-at (point)))

(require 'hide-mode-line)
(hide-mode-line)

(setq compilation-auto-jump-to-first-error t)
(setq compilation-scroll-output t)

(add-hook
  'after-save-hook
  (lambda ()
    (when (string= (buffer-file-name) "c:/home/nweibull/projects/dot-home/emacs")
      (compile "make -k install"))))

(setq compilation-finish-functions 'compilation-finish-autoclose)
(defun compilation-finish-autoclose (buffer string)
  (when (not compilation-current-error)
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

(desktop-save-mode 1)
(setq history-length 512)
(dolist (variable '(command-history
                    read-expression-history
                    viper-quote-region-history
                    viper-search-history))
  (add-to-list 'desktop-globals-to-save variable))

(lexical-let ((global-desktop-dirname (expand-file-name "~/.local/var/cache/emacs")))
  (setq desktop-path (list "." global-desktop-dirname))
  (defun desktop-save-globally ()
    (interactive)
    (setq desktop-dirname global-desktop-dirname))
  (defun desktop-save-locally ()
    (interactive)
    (setq desktop-dirname (expand-file-name "."))))
(desktop-save-globally)

(setq keyboard-coding-system 'utf-8)

(setq digraph-table-user
  '((?\( ?/ ?\x2209)
    (?. ?3 ?\x2026)
    (?, ?3 ?\x22ef)
    (?< ?Y ?\x227a)
    (?< ?/ ?\x27e8)
    (?> ?/ ?\x27e9)))

(require 'digraph)
(define-key viper-insert-global-user-map "\C-k" 'digraph-read)

(require 'cygwin-mount)
(cygwin-mount-activate)

;; (define-key viper-insert-global-user-map "\C-g" 'viper-intercept-ESC-key)

(require 'ned-info-on-file)

;(require 'anything)
(require 'anything-config)
(add-to-list 'anything-sources 'anything-c-source-files-in-current-dir)
(require 'ido)
(ido-mode t)
;(ido-everywhere t)
;(setq ido-enable-flex-matching t)
;(setq ido-use-filename-at-point t)
;(setq ido-auto-merge-work-directories-length -1)
(add-hook 'ido-setup-hook
          (lambda ()
            (define-key ido-completion-map "\C-p" 'ido-prev-match)
            (define-key ido-completion-map "\C-n" 'ido-next-match)
            (define-key ido-completion-map [remap backward-delete-char-untabify] 'ido-delete-backward-updir)))


;; Clear echo area.
(message "")

;set nostartofline
;set path+=./**
;let &listchars .= (&encoding == 'utf-8' ? ',tab:»·,trail:·' : ',tab:>.,trail:.')
;set statusline=%F%(\ [%1*%M%*%(,%2*%R%*%)]%)\ %w%=%(\ %y%)\ line:\ %l\ of\ %L
;set textwidth=79
;set formatlistpat=^\\s*\\%(\\d\\+[\\]:.)}\\t\ ]\\\|[•‣][\\t\ ]\\)\\s*
;set completeopt=menu,menuone,preview
;set expandtab softtabstop=8
;set shiftround
;set autoindent
;set cinoptions=:0,l1,t0,c0,C1,(0,u0
;
;noremap <silent> k :<C-U>call feedkeys(v:count1 . 'n', 'nt')<CR>
;noremap <silent> K :<C-U>call feedkeys(v:count1 . 'N', 'nt')<CR>
;
;call s:map_swap_both_cases(['<C-W>t', '<C-W>j'], ['<C-W><C-T>', '<C-W><C-J>'])
;call s:map_swap_both_cases(['<C-W>n', '<C-W>k'], ['<C-W><C-N>', '<C-W><C-K>'])
;call s:map_swap_both_cases(['<C-W>s', '<C-W>l'], ['<C-W><C-S>', '<C-W><C-L>'])
;call s:map_swap_list([['zt', 'zj'], ['zn', 'zk'], ['zs', 'zl']])
;noremap zS zL
;noremap zK zN
;noremap gt gj
;noremap gn gk
;
;delfunction s:map_swap_both_cases
;delfunction s:map_swap_list
;delfunction s:map_swap
;
;noremap <Space> <C-F>
;ounmap <Space>
;noremap <Backspace> <C-B>
;omap <Backspace> <Delete>
;
;inoremap <silent> <C-Y> <C-R>=pumvisible() ? "\<lt>C-Y>" : "\<lt>C-R>\<lt>C-O>*"<CR>
;inoremap <silent> <Tab> <C-R>=pumvisible() ? "\<lt>C-Y>" : "\<lt>Tab>"<CR>
;
;for digit in [1, 2, 3, 4, 5, 6, 8, 9]
;  execute 'inoremap <silent> ' . digit . ' <C-R>=pumvisible() ? "' . repeat('\<lt>C-N>', digit) . '" : "' . digit . '"<CR>'
;endfor
;
;noremap <Leader>p :cprevious<CR>
;noremap <Leader>n :cnext<CR>
;noremap <Leader>P :cpfile<CR>
;noremap <Leader>N :cnfile<CR>
;
;noremap <silent> <Leader>h <Esc>:set invhlsearch<CR>
;
;noremap <silent> <Leader>s <Esc>:setlocal invspell spelllang=en_us<CR>
;
;nnoremap <Leader>c :cd %:p:h<CR>:pwd<CR>
;nnoremap <Leader>C :lcd %:p:h<CR>:pwd<CR>
;nnoremap <Leader>e :e <C-R>=expand('%:p:h')<CR>/<C-Z>
;nnoremap <Leader>E :e <C-Z>
;
;nnoremap <silent> ,k :bn <Bar> :bd #<CR>
;
;cnoremap <C-A>  <Home>
;cnoremap <C-B>  <Left>
;cnoremap <C-D>  <Delete>
;cnoremap <C-F>  <Right>
;cnoremap <C-N>  <Down>
;cnoremap <C-P>  <Up>
;cnoremap <Esc>b <S-Left>
;cnoremap <Esc>f <S-Right>
;cmap     <Esc>d <C-\>e<SID>command_line_delete_word_to_right()<CR>
;cnoremap <expr> <C-S> (getcmdtype() == '/' \|\| getcmdtype() == '?') ? '<Return>' . getcmdtype() . '<C-R>/' : ""
;cnoremap <expr> <C-O> (getcmdtype() == '/' \|\| getcmdtype() == '?') ? '<Return>' . (getcmdtype() == '/' ? '?' : '/') . '<C-R>/' : ""
;cnoremap <C-Y> <C-R><C-O>*
;
;function! s:command_line_delete_word_to_right()
;  let cmd = getcmdline()
;  let pos = getcmdpos()
;  let before = strpart(cmd, 0, pos - 1)
;  let after = substitute(strpart(cmd, pos), '^\s*\w\+\ze\%(\s\+\|$\)', "", "")
;  return before . after
;endfunction
;
;noremap <silent> g: <Esc>:set operatorfunc=<SID>get_command_mode_range<CR>g@
;
;function! s:get_command_mode_range(type)
;  let b = line("'[")
;  let e = line("']")
;
;  if b < e
;    let range = '.,+' . (e - b)
;  elseif b == e
;    let range = '.'
;  else
;    let range = '.,+' . (b - e)
;  endif
;
;  call inputsave()
;  call feedkeys(':' . range . "\<C-R>=''[inputrestore()]\<CR>", 'n')
;endfunction
;
; (defconst use-backup-dir t)    
; (setq backup-directory-alist (quote ((".*" . "~/.backups/"))) 
;       version-control t		       ; Use version numbers for backups 
;       kept-new-versions 16	       ; Number of newest versions to keep 
;       kept-old-versions 2	       ; Number of oldest versions to keep 
;       delete-old-versions t            ; Delete excess backup versions 
;       history-delete-duplicates t      ; Delete dups in history 
;       history-length 100               ; Larger history size than default 30 
;       dired-listing-switches "-l"      ; Don't display dot files 
;       dired-recursive-deletes 'top     ; Recursive deletes 
;       dired-recursive-copies 'top      ; Recursive copies 
;       backup-by-copying-when-linked t) ; Copy linked files, don't rename
;  
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
