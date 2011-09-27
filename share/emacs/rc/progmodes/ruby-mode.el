(add-hook 'ruby-mode-hook
          '(lambda ()
             (define-key evil-normal-state-local-map ",t" 'ruby-find-other-file)
             (define-key evil-normal-state-local-map ",M" 'ruby-run-test-at-line)
             (define-key evil-insert-state-local-map "d" 'ruby-electric-end-character)
             (define-key evil-insert-state-local-map "e" 'ruby-electric-end-character)
;             (define-key evil-insert-state-local-map "\C-m" 'reindent-then-newline-and-indent)
             (set (make-local-variable 'evil-shift-width) 2)
             (set (make-local-variable 'compile-command) "rake -s ")
             (set (make-local-variable 'compilation-mode-makefile-name) "Rakefile")))

(add-to-list 'compilation-error-regexp-alist-alist
                '(lookout-info "^\tfrom \\([^ \t\r\n]+\\):\\([0-9]+\\):in `[^']*'$" 1 2 nil 0))
(add-to-list 'compilation-error-regexp-alist-alist
                '(lookout "^\\([^: \t\r\n]+\.rb\\):\\([0-9]+\\): " 1 2))
(add-to-list 'compilation-error-regexp-alist 'lookout)
(setq-default compilation-error-regexp-alist '(lookout lookout-info))

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

(defun* ruby-find-other-file (&optional (file-name (buffer-file-name)))
  (interactive)
  (let ((other-file-name (and file-name
                              (or (ruby-find-other-file-name file-name ruby-unit-test-file-name-mapping nil)
                                  (ruby-find-other-file-name file-name ruby-implementation-file-name-mapping)))))
    (if (not other-file-name)
        (signal 'file-error (list "No other file for this buffer" file-name))
      (find-file other-file-name))))

(defun* ruby-find-other-file-name (file-name mapping &optional (must-exist t))
  (let ((replacement
         (loop for e in mapping
               if (and (string-match (car e) file-name)
                       (or (not must-exist)
                           (file-exists-p
                            (replace-match (cdr e) nil nil file-name nil))))
               return (cdr e))))
    (if replacement
        (replace-match replacement nil nil file-name nil))))

(defun* ruby-implementation-file-name-p (&optional file-name (buffer-file-name))
  (ruby-find-other-file-name file-name ruby-unit-test-file-name-mapping nil))

(defun* ruby-unit-test-file-name-p (&optional file-name (buffer-file-name))
  (ruby-find-other-file-name file-name ruby-implementation-file-name-mapping nil))

; TODO: Validate file-name
(defun* ruby-run-test-at-line (&optional (file-name (buffer-file-name)) (line (count-lines (point-min) (point))))
  "Run test at LINE."
  (interactive)
  (let ((test-file-name (if (ruby-implementation-file-name-p file-name)
                            (ruby-find-other-file-name file-name ruby-unit-test-file-name-mapping)
                          file-name))
        (line-as-string (if (ruby-unit-test-file-name-p file-name) (number-to-string line))))
    (message file-name)
    (message test-file-name)
    (message (ruby-implementation-file-name-p file-name))
    (compile-package-immediately
     (concat
      "rake -s"
      " TEST=" test-file-name
      (if line-as-string (concat " LINE=" line-as-string) "")))))

(defun ruby-electric-end-character (arg)
  (interactive "P")
  (self-insert-command (prefix-numeric-value arg))
  (ruby-electric-possibly-adjust-indent))

(defun ruby-electric-possibly-adjust-indent ()
  (if (ruby-electric-adjustable-word-p)
    (save-excursion
      (ruby-indent-line t))))

(defun ruby-electric-code-at-point-p ()
  (let* ((properties (text-properties-at (point))))
    (and (null (memq 'font-lock-string-face properties))
         (null (memq 'font-lock-comment-face properties)))))

(defun ruby-electric-adjustable-word-p ()
  (if (ruby-electric-code-at-point-p)
    (save-excursion
      (beginning-of-line)
      (looking-at "\\s-*\\(else\\|end\\|ensure\\|rescue\\)"))))

;(require 'flymake)

;(defun flymake-ruby-init ()
;  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
;                       'flymake-create-temp-inplace))
;         (local-file  (file-relative-name
;                       temp-file
;                       (file-name-directory buffer-file-name))))
;    (list "ruby" (list "-wc" local-file))))
;
;(push '(".+\\.rb$" flymake-ruby-init) flymake-allowed-file-name-masks)
;(push '("Rakefile$" flymake-ruby-init) flymake-allowed-file-name-masks)
;
;;; TODO: I mean, shouldn't this be local to each flymake-file-name-mask?  This
;;; is rather stupid.
;(push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3) flymake-err-line-patterns)
;
;(add-hook 'ruby-mode-hook
;          '(lambda ()
;             ;; Don't want flymake mode for ruby regions in rhtml files and also on read only files
;             (if (and (not (null buffer-file-name)) (file-writable-p buffer-file-name))
;                 (flymake-mode))
;             (define-key ruby-mode-map "\C-m" 'ruby-reindent-then-newline-and-indent)
;             ))
;
