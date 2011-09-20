;(require 'ruby-mode)
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
;             (define-key ruby-mode-map "d" 'ruby-electric-end-character)
;             (define-key ruby-mode-map "e" 'ruby-electric-end-character)
;(defun ruby-electric-end-character (arg)
;  (interactive "P")
;  (self-insert-command (prefix-numeric-value arg))
;  (ruby-electric-possibly-adjust-indent))
;
;(defun ruby-electric-possibly-adjust-indent ()
;  (if (ruby-electric-adjustable-word-p)
;    (save-excursion
;      (ruby-indent-line t))))
;
;(defun ruby-electric-code-at-point-p()
;  (let* ((properties (text-properties-at (point))))
;    (and (null (memq 'font-lock-string-face properties))
;         (null (memq 'font-lock-comment-face properties)))))
;
;(defun ruby-electric-adjustable-word-p ()
;  (if (ruby-electric-code-at-point-p)
;    (save-excursion
;      (beginning-of-line)
;      (looking-at "\\s-*\\(else\\|end\\)"))))
