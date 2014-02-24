(eval-when-compile
  (require 'cl))

(defun ruby-electric-end-character (arg)
  (interactive "P")
  (self-insert-command (prefix-numeric-value arg))
  (ruby-electric-possibly-adjust-indent))

(defun ruby-electric-possibly-adjust-indent ()
  (if (ruby-electric-adjustable-word-p)
    (save-excursion
      (ruby-indent-line t))))

(defun ruby-electric-adjustable-word-p ()
  (if (ruby-electric-code-at-point-p)
    (save-excursion
      (beginning-of-line)
      (looking-at "\\s-*\\(else\\|elsif\\|end\\|ensure\\|rescue\\)"))))

(defun ruby-electric-code-at-point-p ()
  (let ((properties (text-properties-at (point))))
    (and (null (memq 'font-lock-string-face properties))
         (null (memq 'font-lock-comment-face properties)))))

(dolist (key '("d" "e" "f"))
  (define-key ruby-mode-map key 'ruby-electric-end-character))

(eval-after-load 'evil
  '(evil-define-key 'normal ruby-mode-map
     ",t" 'ruby-find-other-file
     ",M" 'ruby-run-test-at-line))

(defvar compilation-mode-makefile-name)
(add-hook 'ruby-mode-hook
          (lambda ()
            (hs-minor-mode 1)
            (set (make-local-variable 'compile-command) "rake -s ")
            (set (make-local-variable 'compilation-mode-makefile-name) "Rakefile")
            (set (make-local-variable 'paragraph-start) "\f\\|[ \t]*$\\|[ \t]*#[ \t]*@[[:alpha:]]+[ \t]")
            (set (make-local-variable 'adaptive-fill-function)
                 (lambda ()
                   (if (looking-at "\\([ \t]*#[ \t]*\\)@[[:alpha:]]+[ \t]")
                       (concat (match-string 1) "  "))))))

(defcustom ruby-unit-test-file-name-mapping
  '(("\\(.*\\)\\(/lib/\\)\\(.*\\.\\(rb\\|treetop\\)\\)$" . "\\1/test/unit/\\3"))
  "Unit test file-name mapping."
  :type '(alist :value-type (group string))
  :group 'ruby)

(defcustom ruby-implementation-file-name-mapping
  '(("\\(.*\\)\\(/test/unit/\\)\\(.*\\)\\.rb$" . "\\1/lib/\\3.treetop")
    ("\\(.*\\)\\(/test/unit/\\)\\(.*\\)\\.rb$" . "\\1/lib/\\3.rb"))
  "Unit test file-name mapping."
  :type '(alist :value-type (group string))
  :group 'ruby)

(defun ruby-find-other-file (&optional file-name)
  (interactive)
  (let* ((file-name (or file-name (buffer-file-name)))
         (other-file-name (and file-name
                               (or (ruby-find-other-file-name file-name ruby-unit-test-file-name-mapping t)
                                   (ruby-find-other-file-name file-name ruby-implementation-file-name-mapping)))))
    (if (not other-file-name)
        (signal 'file-error (list "No other file for this buffer" file-name))
      (find-file other-file-name))))

(defun ruby-find-other-file-name (file-name mapping &optional missing-ok)
  (let ((replacement
         (loop for e in mapping
               if (and (string-match (car e) file-name)
                       (or missing-ok
                           (file-exists-p
                            (replace-match (cdr e) nil nil file-name nil))))
               return (cdr e))))
    (if replacement
        (replace-match replacement nil nil file-name nil))))

; TODO Remove this once we factor out compile-package into its own feature
; (with autoload).
(declare-function compile-package-immediately "init.el")
; TODO Validate file-name
(defun ruby-run-test-at-line (&optional file-name line)
  "Run test at LINE."
  (interactive)
  (let* ((file-name (or file-name (buffer-file-name)))
         (test-file-name (if (ruby-find-other-file-name file-name ruby-unit-test-file-name-mapping t)
                             (ruby-find-other-file-name file-name ruby-unit-test-file-name-mapping)
                           file-name))
         (line (or line (count-lines (point-min) (point))))
         (line-as-string (if (ruby-find-other-file-name file-name ruby-implementation-file-name-mapping)
                             (number-to-string line))))
    (compile-package-immediately
     (concat
      "rake -s"
      " TEST=" test-file-name
      (if line-as-string (concat " LINE=" line-as-string) "")))))

(defun ruby-file-name-to-module-name (&optional file-name)
  (let ((file-name (or file-name (buffer-file-name))))
    (mapconcat 'identity
               (mapcar 'capitalize
                       (split-string
                        (file-name-sans-extension
                         (or (and file-name
                                  (file-relative-name
                                   file-name
                                   (expand-file-name
                                    (concat
                                     (locate-dominating-file file-name ".git")
                                     "lib"))))
                             (buffer-name)))
                        "/"
                        t))
               "::")))

(define-abbrev ruby-mode-abbrev-table "d" "" 'ruby-skeleton-def)

(define-skeleton ruby-skeleton-def
  "Insert a method definition."
  "Method name and argument list: "
  > "def " str \n
  > _ \n
  "end" >)

(define-abbrev ruby-mode-abbrev-table "tlc" "" 'ruby-skeleton-top-level-class)

(define-skeleton ruby-skeleton-top-level-class
  "Insert a top-level class."
  ""
  "# -*- coding: utf-8 -*-" \n
  \n
  > "class " (ruby-file-name-to-module-name) \n
  > _ \n
  "end" >)

(define-abbrev ruby-mode-abbrev-table "tlm" "" 'ruby-skeleton-top-level-module)

(define-skeleton ruby-skeleton-top-level-module
  "Insert a top-level module."
  ""
  "# -*- coding: utf-8 -*-" \n
  \n
  "module " (ruby-file-name-to-module-name) \n
  > _ \n
  "end" >)

(define-abbrev ruby-mode-abbrev-table "tle" "" 'ruby-skeleton-top-level-expectations)

(define-skeleton ruby-skeleton-top-level-expectations
  "Insert top-level expectations."
  ""
  "# -*- coding: utf-8 -*-" \n
  \n
  "Expectations do" \n
  > "expect " _ " do" \n
  >  _ \n
  "end" > \n
  "end" >)
