(defun tabulated-list-beginning ()
  (goto-char (point-min))
  (unless tabulated-list-use-header-line
    (forward-line)))

(defun tabulated-list-map-entries (function)
  "TBD."
  (save-excursion
    (tabulated-list-beginning)
    (let ((tabulated-list-map-entries-count 0))
      (while (not (eobp))
        (let ((id (tabulated-list-get-id))
              (entry (tabulated-list-get-entry)))
          (if (null entry)
              (forward-line 1)
            (let ((result (save-excursion (funcall function id entry))))
              (cond ((null result)
                     (forward-line 1))
                    ((eq result 'kill)
                     (setq tabulated-list-map-entries-count
                           (+ 1 tabulated-list-map-entries-count))
                     (tabulated-list-delete-entry))
                    (t
                     (setq tabulated-list-map-entries-count
                           (+ 1 tabulated-list-map-entries-count))
                     (forward-line 1)))))))
      tabulated-list-map-entries-count)))

(defcustom Buffer-menu-help-buffer-modes
  '(apropos-mode help-mode Info-mode Info-edit-mode Man-mode woman-mode)
  "List of \"Help\" major modes."
  :type '(repeat function)
  :group 'Buffer-menu)

(defcustom Buffer-menu-compressed-file-name-regexp
  "\\.\\(7z\\|arj\\|bgz\\|bz2\\|gz\\|lzh\\|taz\\|tgz\\|xz\\|zip\\|z\\)$"
  "Regexp to match compressed file names."
  :type 'regexp
  :group 'Buffer-menu)

(defcustom Buffer-menu-old-time 72
  "The number of hours before a buffer is considered \"old\"."
  :type '(choice (const :tag "72 hours (3 days)" 72)
                 (const :tag "48 hours (2 days)" 48)
                 (const :tag "24 hours (1 day)" 24)
                 (integer :tag "hours"))
  :group 'Buffer-menu)

(defcustom Buffer-menu-case-fold-search case-fold-search
  "If non-nil, ignore case when searching."
  :type 'boolean
  :group 'Buffer-menu)

(defun Buffer-menu-map-entries (func)
  "TBD."
  (tabulated-list-map-entries
   (lambda (buffer entry)
     (let ((result (save-excursion (funcall func buffer entry))))
       (when (and (eq result 'kill) (not (eq buffer (current-buffer))))
         (kill-buffer buffer))
       result))))

(defun Buffer-menu-mark-by (fun)
  "TBD."
  (message "%d buffers marked."
           (Buffer-menu-map-entries
            (lambda (buffer _entry)
              (when (funcall fun buffer)
                (tabulated-list-set-col 0 ">" t)
                t)))))

;;;###autoload
(defun Buffer-menu-mark-by-name-regexp (regexp)
  "Mark all buffers whose name matches REGEXP."
  (interactive "sMark buffers (regexp): ")
  (Buffer-menu-mark-by (lambda (b) (string-match regexp (buffer-name b)))))

;;;###autoload
(defun Buffer-menu-mark-by-mode-regexp (regexp)
  "Mark all buffers whose major mode matches REGEXP."
  (interactive "sMark buffers by major mode (regexp): ")
  (Buffer-menu-mark-by
   (lambda (b)
     (with-current-buffer b
       (string-match regexp (format-mode-line mode-name nil nil b))))))

;;;###autoload
(defun Buffer-menu-mark-by-file-name-regexp (regexp)
  "Mark all buffers whose file name matches REGEXP."
  (interactive "sMark buffers by file name (regexp): ")
  (Buffer-menu-mark-by
   (lambda (b)
     (let ((n (or (buffer-file-name b)
                  (with-current-buffer b
                    (and (eq major-mode 'dired)
                         (boundp 'dired-directory)
                         (stringp dired-directory)
                         dired-directory)))))
         (when n
           (string-match regexp n))))))

;;;###autoload
(defun Buffer-menu-mark-by-mode (mode)
  "Mark all buffers whose major mode equals MODE."
  (interactive
   (let* ((b (Buffer-menu-buffer))
          (default (if b (symbol-name (buffer-local-value 'major-mode b)))))
     (list (intern
            (completing-read
             (if default
                 (format "Mark buffers by major mode (default %s): " default)
               "Mark buffers by major mode: ")
             (Buffer-menu-buffer-modes) nil t nil nil default)))))
  (Buffer-menu-mark-by (lambda (b) (eq (buffer-local-value 'major-mode b) mode))))

;;;###autoload
(defun Buffer-menu-mark-modified-buffers ()
  "Mark all modified buffers."
  (interactive)
  (Buffer-menu-mark-by (lambda (b) (buffer-modified-p b))))

;;;###autoload
(defun Buffer-menu-mark-unsaved-buffers ()
  "Mark all modified buffers that have an associated file."
  (interactive)
  (Buffer-menu-mark-by
   (lambda (b)
     (and (buffer-local-value 'buffer-file-name b) (buffer-modified-p b)))))

;;;###autoload
(defun Buffer-menu-mark-dissociated-buffers ()
  "Mark all buffers whose associated file does not exist."
  (interactive)
  (Buffer-menu-mark-by
   (lambda (b)
     (with-current-buffer b
       (or (and buffer-file-name
                (not (file-exists-p buffer-file-name)))
           (and (eq major-mode 'dired-mode)
                (boundp 'dired-directory)
                (stringp dired-directory)
                (not (file-exists-p (file-name-directory dired-directory)))))))))

;;;###autoload
(defun Buffer-menu-mark-help-buffers ()
  "Mark buffers like *Help*, *Apropos*, *Info*."
  (interactive)
  (Buffer-menu-mark-by
   (lambda (b)
     (with-current-buffer b (memq major-mode Buffer-menu-help-buffer-modes)))))

;;;###autoload
(defun Buffer-menu-mark-compressed-file-buffers ()
  "Mark buffers whose associated file is compressed."
  (interactive)
  (Buffer-menu-mark-by
   (lambda (b)
     (with-current-buffer b
       (and buffer-file-name
            (string-match Buffer-menu-compressed-file-name-regexp
                          buffer-file-name))))))

;;;###autoload
(defun Buffer-menu-mark-old-buffers ()
  "Mark buffers which have not been viewed in `Buffer-menu-old-time' hours."
  (interactive)
  (Buffer-menu-mark-by
   (lambda (b)
     (with-current-buffer b
       (when buffer-display-time
         (> (- (float-time) (float-time buffer-display-time))
            (* 60 60 Buffer-menu-old-time)))))))

;;;###autoload
(defun Buffer-menu-mark-special-buffers ()
  "Mark all buffers whose name begins and ends with '*'."
  (interactive)
  (Buffer-menu-mark-by-name-regexp "^\\*.+\\*$"))

;;;###autoload
(defun Buffer-menu-mark-read-only-buffers ()
  "Mark all read-only buffers."
  (interactive)
  (Buffer-menu-mark-by (lambda (b) (buffer-local-value 'buffer-read-only b))))

;;;###autoload
(defun Buffer-menu-mark-dired-buffers ()
  "Mark all `dired' buffers."
  (interactive)
  (Buffer-menu-mark-by
   (lambda (b) (eq (buffer-local-value 'major-mode b) 'dired-mode))))

;;;###autoload
(defun Buffer-menu-change-marks (&optional old new)
  "Change all OLD marks to NEW marks.
OLD and NEW are both characters used to mark buffers."
  (interactive
   (let* ((cursor-in-echo-area t)
          (old (progn (message "Change (old mark): ") (read-char)))
          (new (progn (message  "Change %c marks to (new mark): " old)
                      (read-char))))
     (list old new)))
  (if (or (eq old ?\r) (eq new ?\r))
      (ding)
    (Buffer-menu-map-entries
     (lambda (_b e)
       (when (string-equal (aref e 0) (char-to-string old))
         (tabulated-list-set-col 0 (char-to-string new) t))))))

;;;###autoload
(defun Buffer-menu-unmark-all ()
  "Unmark all buffers."
  (interactive)
  (Buffer-menu-map-entries (lambda (_b _e) (tabulated-list-set-col 0 " " t))))

;;;###autoload
(defun Buffer-menu-do-query-replace-regexp (from to &optional delimited)
  "Do `query-replace-regexp' of FROM with TO in all marked buffers.
If DELIMITED (prefix arg), replace only word-delimited matches."
  (interactive (query-replace-read-args
                (concat "Query replace"
                        (if current-prefix-arg " word" "")
                        " regexp in marked buffers")
                t t))
  (let ((buffers (Buffer-menu-marked-buffers)))
    (dolist (b buffers)
      (with-current-buffer b
        (barf-if-buffer-read-only)))
    (save-window-excursion
      (dolist (b buffers)
        (switch-to-buffer b)
        (save-excursion
          (let ((case-fold-search Buffer-menu-case-fold-search))
            (goto-char (point-min))
            (perform-replace from to t t delimited nil
                             multi-query-replace-map)))))))

;;;###autoload
(defun Buffer-menu-execute-extended-command (prefixarg &optional command-name
                                                       typed)
  (interactive
   (let ((execute-extended-command--last-typed nil))
     (list current-prefix-arg
           (read-extended-command)
           execute-extended-command--last-typed)))
  (let ((function (and (stringp command-name) (intern-soft command-name)))
        (buffers (Buffer-menu-marked-buffers)))
    (unless (commandp function)
      (error "`%s' is not a valid command name" command-name))
    (save-window-excursion
      (dolist (b buffers)
        (switch-to-buffer b)
        (save-excursion
          (set--this-command-keys (concat "\M-x" (symbol-name function) "\r"))
          (setq this-command function)
          (setq real-this-command function)
          (let ((prefix-arg prefixarg))
            (command-execute function nil)))))))

;; TODO Is there no (uniq)?
(defun Buffer-menu-buffer-modes ()
  "Create a completion table of buffer modes currently in use."
  (let ((modes))
    (dolist (b (buffer-list))
      (let ((this-mode (buffer-local-value 'major-mode b)))
        (if (not (memq this-mode modes))
            (push this-mode modes))))
    (mapcar #'symbol-name modes)))

(provide 'buff-menu-ext)
