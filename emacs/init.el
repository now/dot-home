;; -*- lexical-binding: t; -*-

(require 'package)
(setq package-quickstart t)
(setf
 (alist-get "melpa" package-archives nil nil #'equal)
 "https://melpa.org/packages/")
(load (concat user-emacs-directory "packages"))

(add-to-list 'load-path (concat user-emacs-directory "site-lisp"))
(require 'userloaddefs)

(load (setq custom-file (concat user-emacs-directory "custom.el")))

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

(dolist (key
         '("s-&"
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
  (keymap-global-unset key t))

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

(dolist (command '(narrow-to-page set-goal-column))
  (put command 'disabled nil))

(dolist (feature-init
         '((calc . #'now-calc-init)
           (cc-mode . #'now-cc-mode-init)
           (compile . #'now-compile-init)
           (dired . #'now-dired-init)
           (disp-table . #'now-disp-table-init)
           (elisp-mode . #'now-elisp-mode-init)
           (grep . #'now-grep-init)
           (isearch . #'now-isearch-init)
           (iso-transl . #'now-iso-transl-init)
           (lisp-mode . #'now-lisp-mode-init)
           (message . #'now-message-init)
           (nxml-mode . #'now-xml-mode-init)
           (rnc-mode . #'now-rnc-mode-init)
           (ruby-mode . #'ruby-mode-init)
           (sql-indent . #'now-sql-indent-init)
           (term/xterm . #'term/now-xterm-init)
           (typescript-mode . #'now-typescript-mode-init)
           (xref . #'now-xref-init)))
  (eval-after-load (car feature-init) (cdr feature-init)))

(dolist (hook
         (remove
          nil
          `((Buffer-menu-mode-hook Buffer-menu-mode-ext hl-line-mode)
            ,(unless noninteractive '(after-init-hook server-start))
            (arc-mode-hook hl-line-mode)
            (emacs-startup-hook
             hide-mode-line-mode now-report-emacs-startup-time)
            (sed-mode-hook now-set-smie-indent-basic-to-2)
            (tabulated-list-mode-hook
             now-tabulated-list-mode-use-global-glyphless-char-display)
            (tar-mode-hook hl-line-mode))))
  (dolist (function (cdr hook))
    (add-hook (car hook) function)))

(add-to-list
 'window-size-change-functions
 'now-set-split-width-threshold-based-on-aspect-ratio)

(advice-add 'bug-reference-fontify :around 'now-disable-case-fold-search-around)

(advice-add 'smie-auto-fill :around 'now-smie-auto-fill)

(push `(,(rx ?. (or "rng" "sch" "xsd") string-end) . xml-mode) auto-mode-alist)

(eval-when-compile
  (require 'find-func))

(require 'desktop)

(setq
 desktop-dirname (car desktop-path)
 find-function-C-source-directory "~/Projects/emacs/src"
 insert-directory-program "a"
 overlay-arrow-string "►"
 process-connection-type nil            ; TODO Why?
 truncate-string-ellipsis "…")

(setq-default semantic-function-argument-separator ", ")

(load-theme 'now t)
