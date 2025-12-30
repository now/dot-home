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

(define-keymap
  :keymap (current-global-map)
  "C-." 'avy-goto-char-timer
  "C->" 'avy-goto-line
  "s-z" 'undo-only
  "s-Z" 'undo-redo)

(define-keymap
  :keymap ctl-x-map
  "," 'hide-mode-line-show-mode-line
  "C-m" 'pp-macroexpand-last-sexp
  "G" 'magit-file-dispatch
  "c" 'now-project-display-compilation)

(dolist (command '(narrow-to-page set-goal-column))
  (put command 'disabled nil))

(dolist (feature-init
         `((calc . ,#'now-calc-init)
           (cc-mode . ,#'now-cc-mode-init)
           (compile . ,#'now-compile-init)
           (dired . ,#'now-dired-init)
           (disp-table . ,#'now-disp-table-init)
           (eglot . ,#'now-eglot-init)
           (elisp-mode . ,#'now-elisp-mode-init)
           (isearch . ,#'now-isearch-init)
           (iso-transl . ,#'now-iso-transl-init)
           (lisp-mode . ,#'now-lisp-mode-init)
           (message . ,#'now-message-init)
           (mule-util . ,#'now-mule-util-init)
           (nxml-mode . ,#'now-nxml-mode-init)
           (rnc-mode . ,#'now-rnc-mode-init)
           (ruby-mode . ,#'now-ruby-mode-init)
           (term/xterm . ,#'term/now-xterm-init)
           (xref . ,#'now-xref-init)))
  (eval-after-load (car feature-init) (cdr feature-init)))

(dolist (hook
         (remove
          nil
          `((Buffer-menu-mode-hook Buffer-menu-mode-ext hl-line-mode now-do-not-show-trailing-whitespace)
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
 )

(setq-default semantic-function-argument-separator ", ")

(load-theme 'now t)
