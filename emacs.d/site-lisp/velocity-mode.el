(defconst velocity-font-lock-keywords
  (list
   (list "##.*\n" '(0 font-lock-comment-face t))
   (list "#\\*\\([^\\*]\\|\\*[^#]\\)*\\*#" '(0 font-lock-comment-face t))))

(defvar velocity-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap used in rnc-mode.")

(defvar velocity-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?' "\"" table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    table)
  "Syntax table in use for velocity-mode buffers.")

;;;###autoload
(define-derived-mode velocity-mode prog-mode "VTL"
  "Major mode for editing Velocity Templating Language templates.
\\{velocity-mode-map}"
  (setq comment-start "##"
        comment-end ""
        comment-start-skip "\\([ \t]*\\)##?[ \t]*"
        font-lock-defaults '((velocity-font-lock-keywords) nil nil ((?_ . "w")))
        local-abbrev-table velocity-mode-abbrev-table))

;;;###autoload
(add-to-list 'auto-mode-alist (cons (purecopy "\\.vm\\'") 'velocity-mode))

(provide 'velocity-mode)
