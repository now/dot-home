;; -*- lexical-binding: t; -*-

(require 'desktop)

(require 'package)
(setq package-quickstart t)
(setf (alist-get "melpa" package-archives nil nil #'equal) "https://melpa.org/packages/")

(let ((packages
       '(("gnu"
          . (avy
             company
             compat
             dash
             eglot
             json-mode
             sed-mode
             sql-indent
             transient))
         ("nongnu"
          . (git-commit
             go-mode
             magit
             magit-section
             markdown-mode
             typescript-mode
             visual-fill-column
             with-editor
             yaml-mode))
         (nil
          . (docker-tramp
             fira-code-mode
             forge
             smtpmail-multi)))))
  (cl-flet ((refresh-when-pinned (package)
              (when (assq package package-pinned-packages)
                (package-read-all-archive-contents))))
    (dolist (pin packages)
      (when (car pin)
        (dolist (package (cdr pin))
          (push (cons package (car pin)) package-pinned-packages))))
    (dolist (package (mapcan #'cdr packages))
      (unless (package-installed-p package)
        (refresh-when-pinned package)
        (if (assq package package-archive-contents)
            (package-install package)
          (package-refresh-contents)
          (refresh-when-pinned package)
          (package-install package))))))

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

(defun now-set-split-width-threshold-based-on-aspect-ratio (_)
  (setq split-width-threshold
        (pcase (cdr (assoc 'geometry (car (display-monitor-attributes-list))))
          ((and `(,_ ,_ ,width ,height) (guard (< width height))) nil)
          (_ 160))))

(add-to-list 'window-size-change-functions 'now-set-split-width-threshold-based-on-aspect-ratio)

(eval-and-compile
  (require 'use-package)
  (setq use-package-compute-statistics nil
        use-package-verbose nil))

;; This comes first, as we want path set early on.
(use-package now-path
  :when (eq (window-system) 'ns))

(use-package css-mode
  :no-require t
  :custom ((css-indent-offset 2)))

(use-package disp-table
  :config (progn
            (defface wrap-glyph
              '((((min-colors 16) (class color))
                 :foreground "blue")
                (t
                 :inverse-video t))
              "Face for wrap glyph."
              :group 'basic-faces)
            (set-display-table-slot standard-display-table 'wrap #x00a0)
            (set-display-table-slot standard-display-table 'selective-display
                                    (vector (make-glyph-code #x2026)))
            (set-display-table-slot standard-display-table 'vertical-border 0)))

;; TODO Customize
(use-package flycheck
  :disabled)

;; TODO Pretty sure that this can be removed.
(use-package grep
  :config (setq grep-regexp-alist
            `((,(concat "^\\(?:"
                        ;; Parse using NUL characters when `--null' is used.
                        ;; Note that we must still assume no newlines in
                        ;; filenames due to "foo: Is a directory." type
                        ;; messages.
                        "\\(?1:[^\0\n]+\\)\\(?3:\0\\)\\(?2:[0-9]+\\)\\(?::\\|\\(?4:\0\\)\\)"
                        "\\|"
                        ;; Fallback if `--null' is not used, use a tight regexp
                        ;; to handle weird file names (with colons in them) as
                        ;; well as possible.  E.g., use [1-9][0-9]* rather than
                        ;; [0-9]+ so as to accept ":034:" in file names.
                        "\\(?1:"
                        "\\(?:[a-zA-Z]:\\)?" ; Allow "C:..." for w32.
                        "[^\n:]+?[^\n/:]\\):[\t ]*\\(?2:[1-9][0-9]*\\)[\t ]*:"
                        "\\)")
               1 2
               ;; Calculate column positions (col . end-col) of first grep match on a line
               (,(lambda ()
                   (when grep-highlight-matches
                     (let* ((beg (match-end 0))
                            (end (save-excursion (goto-char beg) (line-end-position)))
                            (mbeg (text-property-any beg end 'font-lock-face grep-match-face)))
                       (when mbeg
                         (- mbeg beg)))))
                .
                ,(lambda ()
                   (when grep-highlight-matches
                     (let* ((beg (match-end 0))
                            (end (save-excursion (goto-char beg) (line-end-position)))
                            (mbeg (text-property-any beg end 'font-lock-face grep-match-face))
                            (mend (and mbeg (next-single-property-change mbeg 'font-lock-face nil end))))
                       (when mend
                         (- mend beg))))))
               nil nil
               (3 '(face nil display ":"))
               (4 '(face nil display ":")))
              ("^Binary file \\(.+\\) matches$" 1 nil nil 0 1))))

(use-package iso-transl
  :defer t
  :config (progn
            (iso-transl-define-keys (mapcar (lambda (p) (cons (car p) nil)) iso-transl-char-map))
            (setq iso-transl-dead-key-alist nil)
            (defun iso-transl-define-keys (alist)
              (while alist
                (let ((translated-vec (cdr (car alist))))
                  (define-key iso-transl-ctl-x-8-map (car (car alist)) translated-vec))
                (setq alist (cdr alist))))
            (iso-transl-define-keys iso-transl-char-map)
            (setf (alist-get "now" iso-transl-language-alist nil nil #'equal)
                  '((".")
                    (".." . [?·])
                    (".3" . [?…])
                    ("*'" . [?ʹ])
                    ("*\"" . [?ʺ])))
            (iso-transl-set-language "now")))

(use-package js
  :no-require t
  :custom ((js-indent-level 2)))

(use-package message
  :defer t
  :custom ((message-citation-line-format "%N, %Y-%m-%d %R:\n")
           (message-citation-line-function 'message-insert-formatted-citation-line)
           (message-kill-buffer-on-exit t)
           (message-send-mail-function 'smtpmail-multi-send-it)
           (message-subscribed-addresses nil))
  :config (progn
            (define-abbrev message-mode-abbrev-table "br"
              "Best regards,\n  Nikolai"
              nil :system t :case-fixed t)
            (define-abbrev message-mode-abbrev-table "tbr"
              "Thank you and best regards,\n  Nikolai"
              nil :system t :case-fixed t)
            (define-abbrev message-mode-abbrev-table "tsn"
              "Thanks,\n  Nikolai"
              nil :system t :case-fixed t)))

(use-package mule
  :custom ((default-input-method "rfc1345--"))
  :defer t
  :config (progn
            (quail-define-package
             "rfc1345--" "UTF-8" "m-" t
             "Unicode characters input method using a subset of RFC1345.
For example, “&a'” → “á”"
             nil t nil nil nil nil nil nil nil nil t)
            (require 'rfc1345 "quail/rfc1345")
            (let ((map (quail-map)))
              (quail-select-package "rfc1345--")
              (setf (nth 2 quail-current-package) map)
              (quail-define-rules
               ((append . t))
               ("&(-" ?\∈)
               ("&(/" ?\∉)
               ("&cb" ?\•)
               ("&:)" ?\☺)
               ("&:(" ?\☹)
               ("&<3" ?\❤)))))

(use-package semantic/format
  :no-require t
  :config (progn
            (setq-default semantic-function-argument-separator ", ")))

(use-package shr
  :no-require t
  :custom ((shr-use-fonts nil)
           (shr-width nil)
           (shr-bullet "• ")))

(use-package simple
  :no-require t
  :custom ((indent-tabs-mode nil))
  :defer nil)

(use-package smie
  :defer t
  :config (progn
            (advice-add 'smie-auto-fill :around 'now-smie-auto-fill)))

(use-package smtpmail
  :no-require t
  :custom ((smtpmail-queue-dir "~/Maildir/.Queue/cur")))

(use-package smtpmail-multi
  :no-require t
  :custom ((smtpmail-multi-accounts '((disuse . ("now@disu.se"
                                                 "disu.se"
                                                 587
                                                 nil nil nil nil nil))))
           (smtpmail-multi-associations '(("now@disu.se" disuse)))))

(use-package sql-indent
  :custom ((sqlind-basic-offset 8)))

(use-package typescript-mode
  :no-require t
  :after typescript-mode
  :config (dolist (entry '(typescript-tsc
                           typescript-tsc-pretty
                           typescript-tslint
                           typescript-nglint-error
                           typescript-nglint-warning))
            (setq compilation-error-regexp-alist
                  (delq entry compilation-error-regexp-alist))))

(use-package vc-git
  :defer t
  :config (progn
            (defun vc-git-mode-line-string (_) "")))

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

(defun now-bug-reference-fontify-around (next &rest args)
  (let ((case-fold-search nil))
    (apply next args)))

(advice-add 'bug-reference-fontify :around 'now-bug-reference-fontify-around)

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

(with-eval-after-load 'buffer-mode
  (require 'buffer-menu-ext))

(with-eval-after-load 'calc
  (require 'now-calc))

(eval-after-load 'cc-mode #'now-cc-mode-init)

(eval-after-load 'compile #'now-compile-init)

(with-eval-after-load 'dired
  (require 'dired-x)
  (keymap-set dired-mode-map "e" 'now-dired-ediff-files))

(with-eval-after-load 'elisp-mode
  (font-lock-add-keywords
   'emacs-lisp-mode
   `((,(rx
        ?\(
        (group
         (or "cl-assert" "cl-check-type" "error" "signal" "user-error" "warn"))
        symbol-end)
      (1 font-lock-keyword-face)))))

(with-eval-after-load 'isearch
  (keymap-set isearch-mode-map "C-'" 'avy-isearch))

(with-eval-after-load 'lisp-mode
  (define-keymap
    :keymap lisp-mode-shared-map
    "C-c r" 'raise-sexp
    "C-c s" 'delete-pair))

(with-eval-after-load 'magit
  (require 'forge))

(eval-after-load 'nxml-mode #'now-nxml-mode-init)

(eval-after-load 'rnc-mode #'now-rnc-mode-init)

(eval-after-load 'ruby-mode #'now-ruby-mode-init)

(eval-after-load 'term/xterm #'term/now-xterm-init)

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
 calc-show-banner nil)

(load-theme 'now t)

(add-hook 'emacs-startup-hook 'hide-mode-line-mode)
