;; -*- lexical-binding: t -*-

(require 'dired-aux)
(require 'dired-x)
(require 'ediff)

;;;###autoload
(defun now-dired-ediff-files ()
  "Compare two marked files using `ediff-files'.
If only one file is marked, prompt for second file.  The newer of
the two files is passed as the first argument to `ediff-files'."
  (interactive)
  (let ((files (dired-get-marked-files))
        (windows (current-window-configuration)))
    (if (<= (length files) 2)
        (let* ((file1 (car files))
               (file2 (if (cdr files)
                          (cadr files)
                        (read-file-name
                         (format "Diff %s with: " file1)
                         (dired-dwim-target-directory)))))
          (if (file-newer-than-file-p file1 file2)
              (ediff-files file2 file1)
            (ediff-files file1 file2))
          (add-hook 'ediff-after-quit-hook-internal
                    (lambda ()
                      (setq ediff-after-quit-hook-internal nil)
                      (set-window-configuration windows))))
      (error "no more than 2 files should be marked"))))

(defcustom now-dired-keep-marker-rename-shows dired-keep-marker-rename
  "Controls marking of renamed files.
If t, files keep their previous marks when they are renamed.
If a character, renamed files (whether previously marked or not)
are afterward marked with that character.
This option affects only files renamed by `dired-do-rename' and
`dired-do-rename-regexp'.  See `wdired-keep-marker-rename'
if you want to do the same for files renamed in WDired mode."
  :type '(choice (const :tag "Keep" t)
		 (character :tag "Mark" :value ?R))
  :group 'dired-mark)

(defvar dired-one-file)

;;;###autoload
(defun now-dired-do-rename-shows (&optional arg table)
  (interactive "P\nsTable: ")
  (let* ((imdb "^ *\\(.*\\) S[1-9][0-9]*, Ep\\([1-9][0-9]*\\)$")
         (table (or table (current-kill 0 t)))
         (names (seq-map (lambda (columns)
                           (cons (string-to-number (seq-elt columns 0))
                                 (seq-reduce
                                  (lambda (string pair)
                                    (replace-regexp-in-string
                                     (car pair) (cdr pair) string t t))
                                  '(("[:?*\"<>|!#()]" . "")
                                    ("[/\\]" . "-")
                                    ("'" . "’")
                                    (" " . "_")
                                    ("&" . "and")
                                    ("\\.\\.\\." . "…")
                                    ("\\$" . "s"))
                                  (downcase (seq-elt columns 1)))))
                         (if (string-match-p imdb table)
                             (seq-map (lambda (line)
                                        (save-match-data
                                          (string-match imdb line)
                                          (list (match-string 2 line)
                                                (match-string 1 line))))
                                      (seq-filter (lambda (line)
                                                    (string-match-p imdb line))
                                                  (save-match-data
                                                    (split-string table "\n"
                                                                  t))))
                           (seq-map (lambda (columns)
                                      (list (replace-regexp-in-string
                                             "\\`\\([0-9]+\\)\\(?:\\[[0-9]+\\]\\)?\\'"
                                             "\\1" (seq-elt columns 1) t)
                                            (replace-regexp-in-string
                                             "\\`\"\\(.+\\)\".*" "\\1"
                                             (seq-elt columns 2) t)))
                                    (seq-filter (lambda (columns)
                                                  (and (>= (seq-length columns)
                                                           3)
                                                       (string-match-p
                                                        "\\`[0-9]+\\(?:\\[[0-9]+\\]\\)?\\'"
                                                        (seq-elt columns 1))
                                                       (string-match-p
                                                        "\\`\".+\""
                                                        (seq-elt columns 2))))
                                                (seq-map (lambda (line)
                                                           (save-match-data
                                                             (split-string
                                                              line "\t")))
                                                         (save-match-data
                                                           (split-string
                                                            table "\n" t))))))))
         (fn-list (dired-get-marked-files nil arg nil nil t))
         (dired-one-file (and (consp fn-list)
                              (null (cdr fn-list))
                              (car fn-list))))
    (dired-create-files
     'dired-rename-file
     "Rename"
     fn-list
     (lambda (old)
       (save-match-data
         (let* ((relative-old (dired-make-relative old))
                (number-string (replace-regexp-in-string
                                "\\(?:^\\([0-9]+\\)\\|.*?[Eex1-9]\\([0-9]\\{2,\\}\\)\\).*"
                                "\\1\\2" relative-old t))
                (number (when (string-match-p "[0-9]+" number-string)
                          (string-to-number number-string)))
                (extension (replace-regexp-in-string "\\`.*?\\(\\.[^.]+\\)?\\'"
                                                     "\\1" relative-old t))
                (initial (when number
                           (format "%02d-%s%s" number (cdr (assoc number names))
                                   extension))))
           (read-file-name (format "Rename %s to: " relative-old)
                           (dired-dwim-target-directory) nil nil initial))))
     now-dired-keep-marker-rename-shows)))

;;;###autoload
(defun now-dired-init ()
  (keymap-set dired-mode-map "e" 'now-dired-ediff-files))
