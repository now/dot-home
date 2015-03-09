(require 'smie)

(defgroup rnc nil
  "Major mode for editing Relax NG Compact code."
  :prefix "rnc-"
  :group 'languages)

(defcustom rnc-indent-level 2
  "Indentation of Relax NG Compact statements."
  :type 'integer
  :group 'rnc)

(defconst rnc-font-lock-keywords
  (list
   '("\\b\\(?:attribute\\|element\\)\\s-*\\(\\\\?\\sw\\(?:\\sw\\|\\s_\\)*\\)\\b"
     1 font-lock-variable-name-face)
   '("^\\s-*\\(default\\(?:\\s-+namespace\\)?\\|namespace\\|datatypes\\)\\b"
     1 font-lock-preprocessor-face)
   '("^\\s-*\\(\\\\?\\sw\\(?:\\sw\\|\\s_\\)*\\)\\s-*="
     1 font-lock-variable-name-face)
   (list (concat
          "\\b"
          (regexp-opt
           '("empty"
             "list"
             "mixed"
             "string"
             "text"
             "token"))
          "\\b")
         0 'font-lock-type-face)
   (list (concat
          "\\b"
          (regexp-opt
           '("attribute"
             "div"
             "element"
             "external"
             "grammar"
             "include"
             "inherit"
             "notAllowed"
             "parent"
             "start"))
          "\\b")
         0 'font-lock-keyword-face))
  "Keywords to higlight in rnc-mode.")

(defvar rnc-mode-abbrev-table nil
  "Abbreviation table used in rnc-mode.")

(define-abbrev-table 'rnc-mode-abbrev-table ())

(defvar rnc-mode-map nil
  "Keymap used in rnc-mode.")

(unless rnc-mode-map
  (setq rnc-mode-map (make-sparse-keymap)))

(defvar rnc-mode-syntax-table nil
  "Syntax table in use for rnc-mode buffers.")

(unless rnc-mode-syntax-table
  (setq rnc-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?' "\"" rnc-mode-syntax-table)
  (modify-syntax-entry ?\" "\"" rnc-mode-syntax-table)
  (modify-syntax-entry ?# "<" rnc-mode-syntax-table)
  (modify-syntax-entry ?\n ">" rnc-mode-syntax-table)
  (modify-syntax-entry ?\\ "\\" rnc-mode-syntax-table)
  (modify-syntax-entry ?. "_" rnc-mode-syntax-table)
  (modify-syntax-entry ?- "_" rnc-mode-syntax-table)
  (modify-syntax-entry ?_ "_" rnc-mode-syntax-table)
  (modify-syntax-entry ?+ "." rnc-mode-syntax-table)
  (modify-syntax-entry ?* "." rnc-mode-syntax-table)
  (modify-syntax-entry ?? "." rnc-mode-syntax-table)
  (modify-syntax-entry ?& "." rnc-mode-syntax-table)
  (modify-syntax-entry ?| "." rnc-mode-syntax-table)
  (modify-syntax-entry ?= "." rnc-mode-syntax-table)
  (modify-syntax-entry ?~ "." rnc-mode-syntax-table)
  (modify-syntax-entry ?, "." rnc-mode-syntax-table)
  (modify-syntax-entry ?\; "." rnc-mode-syntax-table) ;; This is a fake entry for SMIE
  (modify-syntax-entry ?\( "()" rnc-mode-syntax-table)
  (modify-syntax-entry ?\) ")(" rnc-mode-syntax-table)
;  (modify-syntax-entry ?\{ "(}" rnc-mode-syntax-table)
;  (modify-syntax-entry ?\} "){" rnc-mode-syntax-table)
  (modify-syntax-entry ?\[ "(]" rnc-mode-syntax-table)
  (modify-syntax-entry ?\] ")[" rnc-mode-syntax-table))

(defconst rnc-mode-smie-grammar
  (smie-prec2->grammar
   (smie-bnf->prec2
    '((id)
      (top-level (grammar-content ";" grammar-content))
;      (top-levels (grammar-content))
      ;(decls (decls ";" decls) (decl))
;      (decl ("namespace" id "=" id))
      ;(patterns (patterns ";" patterns) (pattern))
      (grammar-content (id "=" pattern)
                       (id "|=" pattern)
                       (id "&=" pattern))
      (pattern ("element" id "{" pattern "}")
               (pattern "," pattern)
               ("empty"))
      )
    '((assoc ";" ","))
    '((nonassoc "="))
    '((nonassoc "|="))
    '((nonassoc "&="))
    )))

(defun rnc-mode-newline-semi-p ()
  (save-excursion
    (skip-chars-backward " \t")
    (not (or (bolp)
             (memq (char-before) '(?, ?= ?{))))))

(defun rnc-mode-smie-forward-token ()
  (skip-chars-forward " \t")
  (cond
   ((and (looking-at "\n") (rnc-mode-newline-semi-p))
    (if (eolp)
        (forward-char 1)
      (forward-comment 1))
    ";")
   (t
    (smie-default-forward-token))))

(defun rnc-mode-smie-backward-token ()
  (let ((pos (point)))
    (forward-comment (- (point)))
    (cond
     ((and (> pos (line-end-position)) (rnc-mode-newline-semi-p))
      (skip-chars-forward " \t")
      ";")
     (t
      (smie-default-backward-token)))))

(defun rnc-mode-smie-rules (kind token)
  (message "%s: %s" kind token)
  (pcase (cons kind token)
    (`(:elem . basic) rnc-indent-level)
    (`(:after . "=")
     (when (smie-rule-hanging-p)
       rnc-indent-level))
    (`(:before . "{")
     (when (smie-rule-hanging-p)
       (smie-rule-parent)))
    (`(:after . "{")
     rnc-indent-level)
    (`(:after . "element")
     0)
    ))

;;;###autoload
(defun rnc-mode ()
  "Major mode for editing RELAX NG Compact Syntax schemas.
\\{rnc-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map rnc-mode-map)
  (set-syntax-table rnc-mode-syntax-table)
  (setq local-abbrev-table rnc-mode-abbrev-table)
  (set (make-local-variable 'font-lock-defaults)
       '((rnc-font-lock-keywords) nil nil ((?. . "w") (?- . "w") (?_ . "w"))))
  (set (make-local-variable 'comment-start) "#")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-start-skip) "\\([ \t]*\\)##?[ \t]*")
  (smie-setup rnc-mode-smie-grammar #'rnc-mode-smie-rules
              :forward-token #'rnc-mode-smie-forward-token
              :backward-token #'rnc-mode-smie-backward-token)
  (setq mode-name "RNC"
	major-mode 'rnc-mode)
  (run-mode-hooks 'rnc-mode-hook))

;;;###autoload
(add-to-list 'auto-mode-alist (cons (purecopy "\\.rnc\\'") 'rnc-mode))

(provide 'rnc-mode)
