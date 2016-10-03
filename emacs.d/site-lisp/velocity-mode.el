(defconst velocity-font-lock-keywords
  (list
   (list "##.*\n" '(0 font-lock-comment-face t))
   (list "#\\*\\([^\\*]\\|\\*[^#]\\)*\\*#" '(0 font-lock-comment-face t))))

(defvar velocity-mode-abbrev-table nil
  "Abbreviation table used in rnc-mode.")

(define-abbrev-table 'velocity-mode-abbrev-table ())

(defvar velocity-mode-map nil
  "Keymap used in rnc-mode.")

(unless velocity-mode-map
  (setq velocity-mode-map (make-sparse-keymap)))

(defvar velocity-mode-syntax-table nil
  "Syntax table in use for rnc-mode buffers.")

(unless velocity-mode-syntax-table
  (setq velocity-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?' "\"" velocity-mode-syntax-table)
  (modify-syntax-entry ?\" "\"" velocity-mode-syntax-table)
  (modify-syntax-entry ?= "." velocity-mode-syntax-table)
  (modify-syntax-entry ?\( "()" velocity-mode-syntax-table)
  (modify-syntax-entry ?\) ")(" velocity-mode-syntax-table)
  (modify-syntax-entry ?\{ "(}" velocity-mode-syntax-table)
  (modify-syntax-entry ?\} "){" velocity-mode-syntax-table))

;;;###autoload
(defun velocity-mode ()
  "Major mode for editing Velocity Templating Language templates.
\\{velocity-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map velocity-mode-map)
  (set-syntax-table velocity-mode-syntax-table)
  (setq local-abbrev-table velocity-mode-abbrev-table)
  (set (make-local-variable 'font-lock-defaults)
       '((velocity-font-lock-keywords) nil nil ((?_ . "w"))))
  (set (make-local-variable 'comment-start) "##")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-start-skip) "\\([ \t]*\\)##?[ \t]*")
  (setq mode-name "VTL"
	major-mode 'velocity-mode)
  (run-mode-hooks 'velocity-mode-hook))

;;;###autoload
(add-to-list 'auto-mode-alist (cons (purecopy "\\.vm\\'") 'velocity-mode))

(provide 'velocity-mode)
