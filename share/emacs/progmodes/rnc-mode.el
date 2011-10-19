; TODO: \x{…} and """…""" and '''…''' and >>.

(defgroup rnc nil
  "Major mode for editing Relax NG Compact code."
  :prefix "rnc-"
  :group 'languages)

(defcustom rnc-indent-level 2
  "Indentation of Relax NG Compact statements."
  :type 'integer
  :group 'rnc)

(defun rnc-find-column (first start)
  "Find which column to indent to."

  ;; FIXME: backward-sexp doesn't work with unbalanced braces in comments

  (let* (column
	 pos
	 ;; Find start of enclosing block or assignment
	 (token
	  (if (member first '("]" "}" ")"))
	      (progn
		(goto-char (+ start 1))
		(backward-sexp)
		(beginning-of-line)
		(re-search-forward "\\S ")
		(setq pos (point))
		(setq column (- (current-column) 1))
		'lpar)
	    (catch 'done
	      (while (setq pos (re-search-backward "[{}()=]\\|\\[\\|\\]"
						   (point-min) t))
		(let ((c (match-string 0)))
		  (beginning-of-line)
		  (re-search-forward "\\S ")
		  (setq column (- (current-column) 1))
		  (beginning-of-line)
		  (cond
		   ;; Don't match inside comments
		   ;; FIXME: Should exclude matches inside string literals too
		   ((re-search-forward "#" pos t) (beginning-of-line))
		   ;; Skip block
		   ((member c '("]" "}" ")"))
		    (goto-char (+ pos 1))
		    (backward-sexp))

		   ((string= c "=") (throw 'done 'eq))
		   (t (throw 'done 'lpar)))))))))

    (cond
     ((not pos) 0)
     ((member first '("]" "}" ")")) column)
     ((member first '("{" "(")) (+ column rnc-indent-level))

     ;; Give lines starting with an operator a small negative indent.
     ;; This allows for the following indentation style:
     ;;   foo =
     ;;      bar
     ;;    | baz
     ;;    | oof
     ((member first '("," "&" "|")) (+ column (- rnc-indent-level 2)))

     ;; Check if first preceding non-whitespace character was an operator
     ;; If not, this is most likely a new assignment.
     ;; FIXME: This doesn't play well with name classes starting on a new
     ;; line
     ((eq token 'eq)
      (goto-char start)
      (if (and (re-search-backward "[^ \t\n]" (point-min) t)
	       (member (match-string 0) '("&" "|" "," "=" "~")))
	  (+ column rnc-indent-level)
	column))

     (t (+ column rnc-indent-level)))))

(defun rnc-indent-line ()
  "Indents the current line."
  (interactive)
  (let ((orig-point (point)))
    (beginning-of-line)
    (let* ((beg-of-line (point))
	   (pos (re-search-forward "\\(\\S \\|\n\\)" (point-max) t))
	   (first (match-string 0))
	   (start (match-beginning 0))
	   (col (- (current-column) 1)))

      (goto-char beg-of-line)

      (let ((indent-column (rnc-find-column first start)))
	(goto-char beg-of-line)

	(cond
	 ;; Only modify buffer if the line must be reindented
	 ((not (= col indent-column))
	  (if (not (or (null pos)
		       (= beg-of-line start)))
	      (kill-region beg-of-line start))

	  (goto-char beg-of-line)

	  (while (< 0 indent-column)
	    (insert " ")
	    (setq indent-column (- indent-column 1))))

	 ((< orig-point start) (goto-char start))
	 (t (goto-char orig-point)))))))

(defun rnc-electric-brace (arg)
  (interactive "*P")
  (self-insert-command (prefix-numeric-value arg))
  (rnc-indent-line)
  (let ((p (point)))
    (when (save-excursion
            (beginning-of-line)
            (let ((pos (re-search-forward "\\S " (point-max) t)))
              (and pos (= (- pos 1) p))))
      (forward-char))))

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
  (setq rnc-mode-map (make-sparse-keymap))
  (define-key rnc-mode-map "{" 'rnc-electric-brace)
  (define-key rnc-mode-map "}" 'rnc-electric-brace)
  (define-key rnc-mode-map "[" 'rnc-electric-brace)
  (define-key rnc-mode-map "]" 'rnc-electric-brace))

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
  (modify-syntax-entry ?\( "()" rnc-mode-syntax-table)
  (modify-syntax-entry ?\) ")(" rnc-mode-syntax-table)
  (modify-syntax-entry ?\{ "(}" rnc-mode-syntax-table)
  (modify-syntax-entry ?\} "){" rnc-mode-syntax-table)
  (modify-syntax-entry ?\[ "(]" rnc-mode-syntax-table)
  (modify-syntax-entry ?\] ")[" rnc-mode-syntax-table))

;;;###autoload
(defun rnc-mode ()
  "Major mode for editing RELAX NG Compact Syntax schemas.
\\{rnc-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map rnc-mode-map)
  (set-syntax-table rnc-mode-syntax-table)
  (setq local-abbrev-table rnc-mode-abbrev-table)
  (set (make-local-variable 'indent-line-function) 'rnc-indent-line)
  (set (make-local-variable 'font-lock-defaults) '((rnc-font-lock-keywords) nil nil ((?. . "w") (?- . "w") (?_ . "w"))))
  (set (make-local-variable 'comment-start) "#")
  (set (make-local-variable 'comment-end) "")
  ; TODO: Why not use \s- instead of [ \n\t]?
  (set (make-local-variable 'comment-start-skip) "\\([ \t]*\\)##?[ \t]*")
  (setq mode-name "RNC"
	major-mode 'rnc-mode)
  (run-mode-hooks 'rnc-mode-hook))

;;;###autoload
(add-to-list 'auto-mode-alist (cons (purecopy "\\.rnc\\'") 'rnc-mode))

(provide 'rnc-mode)
