(require 'compile)
(require 'grep)

;;;###autoload
(defun now-git-grep (regexp &optional files dir confirm template)
  "Run git grep, searching for REGEXP in FILES in directory DIR.
The search is limited to file names matching shell pattern FILES.
FILES may use abbreviations defined in `grep-files-aliases', e.g.
entering `ch' is equivalent to `*.[ch]'.  As whitespace triggers
completion when entering a pattern, including it requires
quoting, e.g. `\\[quoted-insert]<space>'.

With \\[universal-argument] prefix, you can edit the constructed shell command line
before it is executed.
With two \\[universal-argument] prefixes, directly edit and run `grep-command'.

Collect output in a buffer.  While git grep runs asynchronously, you
can use \\[next-error] (M-x next-error), or \\<grep-mode-map>\\[compile-goto-error] \
in the grep output buffer,
to go to the lines where grep found matches.

This command shares argument histories with \\[rgrep] and \\[grep]."
  (interactive
   (progn
     (grep-compute-defaults)
     (let ((grep-program "git -P grep")
           (grep-host-defaults-alist nil)
           (grep-find-command nil)
           (grep-find-template nil)
           (grep-find-use-xargs nil)
           (grep-highlight-matches t)
           (grep-template nil)
           (grep-use-null-device nil)
           (grep-use-null-filename-separator t))
       (grep-compute-defaults)
       (cond
        ((and grep-command (equal current-prefix-arg '(16)))
         (list (read-from-minibuffer "Run: " grep-command
                                     nil nil 'grep-history)))
        ((not grep-template)
         (error "now-git.el: No `grep-template' available"))
        (t (let* ((regexp (grep-read-regexp))
                  (files (let ((completing-read-function 'completing-read-default))
                           (grep-read-files regexp)))
                  (quoted-files
                   (if (string-equal files
                                     (cdr (assoc "all" grep-files-aliases)))
                       ""
                     (mapconcat #'shell-quote-argument (split-string files)
                                " ")))
                  (dir (read-directory-name "In directory: "
                                            nil default-directory t))
                  (confirm (equal current-prefix-arg '(4))))
             (list regexp quoted-files dir confirm grep-template)))))))
  (when (and (stringp regexp) (> (length regexp) 0))
    (when (and (stringp files) (> (length files) 0))
      (setq files (concat "-- " files)))
    (unless (and dir (file-accessible-directory-p dir))
      (setq dir default-directory))
    (unless template
      (setq template grep-template))
    (let ((grep-highlight-matches 'always)
          (command regexp))
      (if (null files)
          (if (string= command grep-command)
              (setq command nil))
        (setq dir (file-name-as-directory (expand-file-name dir)))
        (setq command (grep-expand-template template regexp files nil nil))
        (when command
          (if confirm
              (setq command
                    (read-from-minibuffer "Confirm: "
                                          command nil nil 'grep-history))
            (add-to-history 'grep-history command))))
      (when command
        (let ((default-directory dir))
          ;; Setting process-setup-function makes exit-message-function work
          ;; even when async processes aren't supported.
          (grep--save-buffers)
          (compilation-start (if (and grep-use-null-device null-device)
                                 (concat command " " null-device)
                               command)
                             'grep-mode))
        ;; Set default-directory if we started git-grep in the *grep* buffer.
        (if (eq next-error-last-buffer (current-buffer))
            (setq default-directory dir))))))
