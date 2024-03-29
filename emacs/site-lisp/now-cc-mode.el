(require 'cc-mode)

(defvar c-syntactic-element)

;;;###autoload
(defun now-c-fill-paragraph (&optional arg)
  "Like `c-fill-paragraph', but add indent to `fill-column'.
The `fill-column' will be set to its current value, plus any
indent, if we're filling a paragraph, though not any higher than
80."
  (interactive "*P")
  (let ((c-lit-limits (c-literal-limits nil t)))
    (when (memq (c-literal-type c-lit-limits) '(c c++))
      (let ((fill-column (min (+ (current-fill-column)
                                 (- (car c-lit-limits) (point-at-bol)))
                              80)))
        (c-fill-paragraph arg)))))

;;;###autoload
(defun now-c-mode-adaptive-fill-function ()
  (when (looking-at "\\([ \t]*\\(?://+\\|\\**\\)[ \t]*\\)\\(?:@[[:alpha:]]+\\|•\\)[ \t]")
    (concat (match-string 1) "  ")))

;;;###autoload
(defun now-c-arglist-intro (langelem)
  (save-excursion
    (goto-char (c-langelem-pos langelem))
    (vector (+ (current-indentation) (* 2 c-basic-offset)))))

;;;###autoload
(defun now-c-lineup-arglist-decl-only (langelem)
  (save-excursion
    (goto-char (c-langelem-pos c-syntactic-element))
    (if (memq (car (car (c-guess-basic-syntax)))
              '(annotation-top-cont inclass))
        (c-lineup-arglist langelem)
      (goto-char (c-langelem-pos langelem))
      (vector (+ (current-indentation) (* 2 c-basic-offset))))))

;;;###autoload
(defun now-c-auto-newline-mode (&optional arg)
  "Toggle auto-newline mode on or off.
With a prefix argument ARG, enable auto-newline mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil, and toggle it if ARG is `toggle'.

Turning on auto-newline mode automatically enables electric
indentation.

When auto-newline mode is on (indicated by “/la” on the mode
line after the mode name) newlines are automatically inserted
after special characters such as brace, comma, semi-colon, and
colon."
  (interactive (list (or current-prefix-arg 'toggle)))
  (c-toggle-auto-newline (if (eq arg 'toggle)
                             (not (and c-auto-newline c-electric-flag))
                           (> (prefix-numeric-value arg) 0))))

;;;#autoload
(defun now-java-mode-clean-up-imports ()
  (interactive)
  (save-match-data
    (save-excursion
      (goto-char (point-min))
      (let* ((case-fold-search)
             (imports (save-excursion
                        (cl-remove-duplicates
                         (cl-loop while (search-forward-regexp
                                         (rx bol
                                             "import"
                                             (+ space)
                                             (? (group "static") (+ space))
                                             (group (+ (not (or ?. ?\n))))
                                             ?.
                                             (* any)
                                             ?.
                                             (group (+ (not (or ?. ?\n))))
                                             ?\;
                                             ?\n)
                                         nil t)
                                  collect (list (match-string 0)
                                                (match-string-no-properties 1)
                                                (match-string-no-properties 2)
                                                (match-string-no-properties 3))
                                  do (progn
                                       (forward-line -1)
                                       (kill-whole-line)))
                         :test (lambda (a b) (string= (nth 0 a) (nth 0 b)))))))
        (search-forward-regexp "^package ")
        (forward-line)
        (while (string-empty-p
                (buffer-substring-no-properties (line-beginning-position)
                                                (line-end-position)))
          (kill-whole-line))
        (newline)
        (let* ((used (cl-remove-if-not
                      (lambda (matches)
                        (or (string= (nth 3 matches) "*")
                            (save-excursion
                              (search-forward-regexp
                               (concat "\\b"
                                       (regexp-quote (nth 3 matches))
                                       "\\b")
                               nil
                               t))))
                      imports))
               (select (lambda (selector imports)
                         (sort (cl-mapcar (lambda (matches) (nth 0 matches))
                                          (cl-remove-if-not
                                           (lambda (matches)
                                             (funcall selector (nth 2 matches)))
                                           imports))
                               'string<)))
               (both (cl-mapcar
                      (lambda (imports)
                        (list
                         (funcall select
                                  (lambda (root)
                                    (not (member root '("java" "javax"))))
                                  imports)
                         (nconc
                          (funcall select
                                   (lambda (root) (string= root "javax"))
                                   imports)
                          (funcall select
                                   (lambda (root) (string= root "java"))
                                   imports))))
                      (cl-mapcar (lambda (f)
                                   (funcall f
                                            (lambda (matches) (nth 1 matches))
                                            used))
                                 '(cl-remove-if cl-remove-if-not)))))
          (dolist (group both)
            (dolist (imports group)
              (when imports
                (apply 'insert imports)
                (newline)))))))))

;;;###autoload
(defun now-cc-mode-init ()
  (keymap-set c-mode-base-map "C-j" #'c-context-line-break)
  (keymap-set c-mode-map "M-q" 'now-c-fill-paragraph)
  (c-add-style
   "now-c"
   `("linux"
     (c-hanging-colons-alist . ((label after)))
     (c-offsets-alist
      .
      ((arglist-cont . +)
       (arglist-cont-nonempty . +)
       (cpp-define-intro . +)
       (inher-cont . +)
       (member-init-cont . +)
       (objc-method-args-cont . +)
       (objc-method-call-cont . +)
       (template-args-cont . +)))
     (comment-start . "// ")
     (comment-end . "")
     (paragraph-start
      .
      ,(rx-let ((form-feed ?\f)
                (any-whitespace (zero-or-more whitespace))
                (empty-line (sequence any-whitespace line-end))
                (comment-starter (or (>= 2 ?/) (one-or-more ?*)))
                (comment-start
                 (sequence any-whitespace comment-starter any-whitespace))
                (tag (sequence ?@ (one-or-more letter)))
                (separate-comment-starter
                 (or line-end (sequence (or tag ?•) whitespace)))
                (separate-comment-start
                 (sequence comment-start separate-comment-starter)))
         (rx (or form-feed empty-line separate-comment-start))))))
  (c-add-style
   "now-java"
   `("java"
     (c-offsets-alist . ((arglist-intro . ++)))
     (whitespace-line-column . 100))))

;;;###autoload
(defun now-c-set-adaptive-fill-function ()
  "Set `adaptive-fill-function' to `now-c-mode-adaptive-fill-function'."
  (setq-local adaptive-fill-function 'now-c-mode-adaptive-fill-function))

(provide 'now-cc-mode)
