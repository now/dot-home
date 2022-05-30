;;; now-ruby.el --- ruby-mode customizations         -*- lexical-binding: t; -*-

;; Copyright © 2022  Nikolai Weibull

;; Author: Nikolai Weibull <now@disu.se>
;; Keywords: local

;;; Code:

(require 'ruby-mode)

(defcustom now-ruby-unit-test-file-name-mapping
  '(("\\(.*\\)\\(?:/lib/\\)\\(.*\\.\\(?:rb\\)\\)$" . "\\1/test/unit/\\2"))
  "Unit test file-name mapping."
  :type '(alist :value-type (group string))
  :group 'ruby)

(defcustom now-ruby-implementation-file-name-mapping
  '(("\\(.*\\)\\(?:/test/unit/\\)\\(.*\\)\\.rb$" . "\\1/lib/\\3.rb"))
  "Unit test file-name mapping."
  :type '(alist :value-type (group string))
  :group 'ruby)

(defun now-ruby-find-other-file (&optional file-name)
  "Find the other file related to FILE-NAME,
that is, the unit test or the implementation, see
`now-ruby-unit-test-file-name-mapping' and
`now-ruby-implementation-file-name-mapping'."
  (interactive)
  (let* ((file-name (or file-name (buffer-file-name)))
         (other-file-name (and file-name
                               (or (now-ruby-find-other-file-name
                                    file-name
                                    now-ruby-unit-test-file-name-mapping
                                    t)
                                   (now-ruby-find-other-file-name
                                    file-name
                                    now-ruby-implementation-file-name-mapping)))))
    (if (not other-file-name)
        (signal 'file-error (list "No other file for this buffer" file-name))
      (find-file other-file-name))))

(defun now-ruby-find-other-file-name (file-name mapping &optional missing-ok)
  "Find the other file for FILE-NAME in MAPPING.
If MISSING-OK is `t', then the other file isn’t required to exist on the
filesystem."
  (let ((replacement
         (cl-loop for e in mapping
               if (and (string-match (car e) file-name)
                       (or missing-ok
                           (file-exists-p
                            (replace-match (cdr e) nil nil file-name nil))))
               return (cdr e))))
    (if replacement
        (replace-match replacement nil nil file-name nil))))

; TODO Validate file-name
(defun now-ruby-run-test-at-line (&optional file-name line)
  "Run test for FILe-NAME at LINE."
  (interactive)
  (let* ((file-name (or file-name (buffer-file-name)))
         (test-file-name (if (now-ruby-find-other-file-name
                              file-name now-ruby-unit-test-file-name-mapping t)
                             (now-ruby-find-other-file-name
                              file-name now-ruby-unit-test-file-name-mapping)
                           file-name))
         (line (or line (count-lines (point-min) (point))))
         (line-as-string (if (now-ruby-find-other-file-name
                              file-name
                              now-ruby-implementation-file-name-mapping)
                             (number-to-string line))))
    (compile
     (concat
      "rake -s"
      " TEST=" test-file-name
      (if line-as-string (concat " LINE=" line-as-string) "")))))

(defun now-ruby-file-name-to-module-name (&optional file-name)
  "Convert FILE-NAME to a Ruby module name."
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

;;;###autoload
(defun now-ruby-mode-init ()
  "Initialize customizations to `ruby-mode'."
  (add-hook 'ruby-mode-hook 'now-ruby-set-compile-command-to-rake)
  (add-hook 'ruby-mode-hook 'now-ruby-include-yard-tags-in-paragraph-start)
  (add-hook 'ruby-mode-hook 'now-ruby-set-adaptive-fill-function)
  (define-abbrev ruby-mode-abbrev-table "d" "" 'ruby-skeleton-def :system t)
  (define-abbrev
    ruby-mode-abbrev-table
    "tlc"
    ""
    'ruby-skeleton-top-level-class
    :system t)
  (define-abbrev
    ruby-mode-abbrev-table
    "tlm"
    ""
    'ruby-skeleton-top-level-module
    :system t)
  (define-abbrev ruby-mode-abbrev-table
    "tle"
    ""
    'ruby-skeleton-top-level-expectations
    :system t))

(define-skeleton ruby-skeleton-def
  "Insert a method definition."
  "Method name and argument list: "
  "def " str \n
  _ \n
  "end" >)

(define-skeleton ruby-skeleton-top-level-class
  "Insert a top-level class."
  nil
  "# -*- coding: utf-8 -*-" \n
  \n
  "class " (ruby-file-name-to-module-name) \n
  _ \n
  "end" >)

(define-skeleton ruby-skeleton-top-level-module
  "Insert a top-level module."
  nil
  "# -*- coding: utf-8 -*-" \n
  \n
  "module " (ruby-file-name-to-module-name) \n
  _ \n
  "end" >)

(define-skeleton ruby-skeleton-top-level-expectations
  "Insert top-level expectations."
  nil
  "# -*- coding: utf-8 -*-" \n
  \n
  "Expectations do" \n
  "expect " _ " do" \n
  _ \n
  "end" \n
  "end" >)

;;;###autoload
(defun now-ruby-set-compile-command-to-rake ()
  "Set `compile-command' to rake -s."
  (setq-local compile-command "rake -s "))

;;;###autoload
(defun now-ruby-include-yard-tags-in-paragraph-start ()
  "Set `paragraph-start' to handle YARD tags."
  (setq-local
   paragraph-start
   (rx-let ((form-feed ?\f)
            (optional-whitespace (zero-or-more whitespace))
            (empty-line (sequence optional-whitespace line-end))
            (comment-start (sequence optional-whitespace ?# optional-whitespace))
            (yard-tag (sequence ?@ (one-or-more letter)))
            (yard-comment-start (sequence comment-start yard-tag whitespace)))
     (rx (or form-feed empty-line yard-comment-start)))))

;;;###autoload
(defun now-ruby-set-adaptive-fill-function ()
  "Set `adaptive-fill-function' to `now-ruby-adaptive-fill-function'."
  (setq-local adaptive-fill-function 'now-ruby-adaptive-fill-function))

(defun now-ruby-adaptive-fill-function ()
  "Value of `adaptive-fill-function' for `ruby-mode'."
  (if (looking-at "\\([ \t]*#[ \t]*\\)@[[:alpha:]]+[ \t]")
      (concat (match-string 1) "  ")))
