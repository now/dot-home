;; -*- lexical-binding: t; -*-

(require 'desktop)

(require 'package)
(setq package-quickstart t)
(setf (alist-get "melpa" package-archives nil nil #'equal) "https://melpa.org/packages/")
(load (concat user-emacs-directory "packages"))

(let ((modes
       '(("\\.rng\\'" . xml-mode)
         ("\\.sch\\'" . xml-mode)
         ("\\.xsd\\'" . xml-mode))))
  (dolist (mode modes)
    (push mode auto-mode-alist)))

(add-to-list 'load-path (concat user-emacs-directory "site-lisp"))
(require 'userloaddefs)

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

;; TODO Why?
(setq process-connection-type nil)

;; This comes first, as we want path set early on.
(when (eq (window-system) 'ns)
  (require 'now-path))

(defun light-or-dark-mode ()
  (interactive)
  (customize-set-variable
   'frame-background-mode
   (if (= (do-applescript
           "tell application \"System Events\"
            if get dark mode of appearance preferences then
              1
            else
              0
            end if
          end tell") 1)
       'dark
     'light))
  (customize-set-variable 'custom-enabled-themes custom-enabled-themes))

(dolist (b '("s-&"
             ;"s-'"
             "s-:"
             "s-C"
             "s-S"
             "s-^"
             ;"s-`"
             "s-o"
             "s-t"
             "C-/"
             "C-?"
             "C-M-<down>"
             "C-M-<down-mouse-1>"
             "C-M-<drag-mouse-1>"
             "C-M-<end>"
             "C-M-<home>"
             "C-M-<left>"
             "C-M-<mouse-1>"
             "C-M-<right>"
             "C-M-<up>"
             "C-<down>"
             "C-<down-mouse-1>"
             "C-<down-mouse-2>"
             "C-<end>"
             "C-<f10>"
             "C-<home>"
             "C-<insert>"
             "C-<insertchar>"
             "C-<left>"
             "C-<next>"
             "C-<prior>"
             "C-<right>"
             "C-<up>"
             "M-<begin>"
             "M-<down-mouse-1>"
             "M-<drag-mouse-1>"
             "M-<end>"
             "M-<f10>"
             "M-<home>"
             "M-<left>"
             "M-<mouse-1>"
             "M-<mouse-2>"
             "M-<mouse-3>"
             "M-<next>"
             "M-<prior>"
             "M-<right>"
             "M-`"
             "S-<f10>"
             "S-<insert>"
             "S-<insertchar>"
             "S-<mouse-1>"
             "S-<mouse-3>"
             "<XF86Back>"
             "<XF86Forward>"
             "<Scroll_Lock>"
             "<again>"
             "<begin>"
             "<down>"
             "<down-mouse-1>"
             "<drag-mouse-1>"
             "<drag-n-drop>"
             "<end>"
             ;"<execute>"
             "<f1>"
             "<f2>"
             "<f3>"
             "<f4>"
             "<f10>"
             "<f11>"
             ;"<find>"
             "<help>"
             "<home>"
             "<insert>"
             "<insertchar>"
             "<kp-end>"
             "<kp-home>"
             "<kp-next>"
             "<kp-prior>"
             "<left>"
             "<menu>"
             ;"<mouse-1>"
             "<mouse-2>"
             "<mouse-3>"
             "<next>"
             "<open>"
             "<pinch>"
             "<prior>"
             "<redo>"
             "<right>"
             "s-<kp-bar>"
             "s-<left>"
             "s-<right>"
             "<up>"
             "ESC C-<down>"
             "ESC C-<end>"
             "ESC C-<home>"
             "ESC C-<left>"
             "ESC C-<right>"
             "ESC C-<up>"
             "ESC <begin>"
             "ESC <end>"
             "ESC <f10>"
             "ESC <home>"
             "ESC <left>"
             "ESC <next>"
             "ESC <prior>"
             "ESC <right>"
             "C-x C-d"
             "C-x 6"
             ;"C-x +"
             ;"C-x -"
             ;"C-x ."
             ;"C-x ;"
             ;"C-x `"
             ;"C-x f"
             "C-x C-<left>"
             "C-x C-<right>"
             "C-x <left>"
             "C-x <right>"
             ))
  (keymap-global-unset b t))

(define-keymap
  :keymap (current-global-map)
  "C-." 'avy-goto-char-timer
  "C->" 'avy-goto-line
  "s-z" 'undo-only
  "s-Z" 'undo-redo)

(keymap-unset help-map "<f1>" t)
(keymap-unset help-map "<help>" t)

(define-keymap
  :keymap ctl-x-map
  "," 'hide-mode-line-show-mode-line
  "C-m" 'pp-macroexpand-last-sexp
  "G" 'magit-file-dispatch
  "c" 'now-project-display-compilation)

(put 'narrow-to-page 'disabled nil)
(put 'set-goal-column 'disabled nil)

(with-eval-after-load 'buffer-menu
  (require 'buffer-menu-ext))

(with-eval-after-load 'calc
  (require 'now-calc))

(eval-after-load 'cc-mode #'now-cc-mode-init)

(eval-after-load 'compile #'now-compile-init)

(eval-after-load 'dired #'now-dired-init)

(eval-after-load 'disp-table #'now-disp-table-init)

(with-eval-after-load 'elisp-mode
  (font-lock-add-keywords
   'emacs-lisp-mode
   `((,(rx
        ?\(
        (group
         (or "cl-assert" "cl-check-type" "error" "signal" "user-error" "warn"))
        symbol-end)
      (1 font-lock-keyword-face)))))

(eval-after-load 'grep #'now-grep-init)

(with-eval-after-load 'isearch
  (keymap-set isearch-mode-map "C-'" 'avy-isearch))

(eval-after-load 'iso-transl #'now-iso-transl-init)

(with-eval-after-load 'lisp-mode
  (define-keymap
    :keymap lisp-mode-shared-map
    "C-c r" 'raise-sexp
    "C-c s" 'delete-pair))

(eval-after-load 'message #'now-message-init)

(eval-after-load 'nxml-mode #'now-nxml-mode-init)

(eval-after-load 'rnc-mode #'now-rnc-mode-init)

(eval-after-load 'ruby-mode #'now-ruby-mode-init)

(eval-after-load 'sql-indent #'now-sql-indent-init)

(eval-after-load 'term/xterm #'term/now-xterm-init)

(eval-after-load 'typescript-mode #'now-typescript-mode-init)

(eval-after-load 'xref #'now-xref-init)

(add-hook 'Buffer-menu-mode-hook 'hl-line-mode)

(unless noninteractive
  (add-hook 'after-init-hook 'server-start))

(add-hook 'arc-mode-hook 'hl-line-mode)

(add-hook 'emacs-startup-hook 'now-report-emacs-startup-time)

(add-hook 'sed-mode-hook 'now-set-smie-indent-basic-to-2)

(add-hook
 'tabulated-list-mode-hook
 'now-tabulated-list-mode-use-global-glyphless-char-display)

(add-hook 'tar-mode-hook 'hl-line-mode)

(add-to-list
 'window-size-change-functions
 'now-set-split-width-threshold-based-on-aspect-ratio)

(advice-add 'bug-reference-fontify :around 'now-disable-case-fold-search-around)

(advice-add 'smie-auto-fill :around 'now-smie-auto-fill)

(eval-when-compile
  (require 'find-func))

(setq
 desktop-dirname (car desktop-path)
 find-function-C-source-directory "~/Projects/emacs/src"
 insert-directory-program "a"
 overlay-arrow-string "►"
 truncate-string-ellipsis "…")

(setq-default
 calc-group-char " "
 calc-gnuplot-default-device "dumb"
 calc-show-banner nil
 semantic-function-argument-separator ", ")

(load-theme 'now t)

(add-hook 'emacs-startup-hook 'hide-mode-line-mode)
