;;; TODO This mode should perhaps be called m4sugar.

(defvar autotest-font-lock-keywords
  `(("\\_<dnl\\_>.*$" . font-lock-comment-face)
    ("\\$[*#@0-9]" . font-lock-variable-name-face)
    ("\\$\\@" . font-lock-variable-name-face)
    ("\\$\\*" . font-lock-variable-name-face)
    (,(regexp-opt '("m4_builtin"
                    "m4_changecom"
                    "m4_changequote"
                    "m4_debugfile"
                    "m4_debugmode"
                    "m4_decr"
                    "m4_define"
                    "m4_divnum"
                    "m4_errprint"
                    "m4_esyscmd"
                    "m4_eval"
                    "m4_format"
                    "m4_ifdef"
                    "m4_incr"
                    "m4_index"
                    "m4_indir"
                    "m4_len"
                    "m4_pushdef"
                    "m4_shift"
                    "m4_substr"
                    "m4_syscmd"
                    "m4_sysval"
                    "m4_traceoff"
                    "m4_traceon"
                    "m4_translit"
                    "__file__"
                    "__line__"
                    "__oline__"
                    "m4_bpatsubst"
                    "m4_bregexp"
                    "m4_copy"
                    "m4_copy_force"
                    "m4_rename"
                    "m4_rename_force"
                    "m4_defn"
                    "m4_divert"
                    "m4_dumpdef"
                    "m4_dumpdefs"
                    "m4_esyscmd_s"
                    "m4_exit"
                    "m4_if"
                    "m4_include"
                    "m4_sinclude"
                    "m4_mkstemp"
                    "m4_maketemp"
                    "m4_popdef"
                    "m4_undefine"
                    "m4_undivert"
                    "m4_wrap"
                    "m4_wrap_lifo"
                    "m4_assert"
                    "m4_errprintn"
                    "m4_fatal"
                    "m4_location"
                    "m4_warn"
                    "m4_cleardivert"
                    "m4_divert_once"
                    "m4_divert_pop"
                    "m4_divert_push"
                    "m4_divert_text"
                    "m4_init"
                    "m4_bmatch"
                    "m4_bpatsubsts"
                    "m4_case"
                    "m4_cond"
                    "m4_default"
                    "m4_default_quoted"
                    "m4_default_nblank"
                    "m4_default_nblank_quoted"
                    "m4_defined_default"
                    "m4_ifblank"
                    "m4_ifnblank"
                    "m4_ifndef"
                    "m4_ifset"
                    "m4_ifval"
                    "m4_ifvaln"
                    "m4_n"
                    "m4_argn"
                    "m4_car"
                    "m4_cdr"
                    "m4_for"
                    "m4_foreach"
                    "m4_foreach_w"
                    "m4_map"
                    "m4_mapall"
                    "m4_map_sep"
                    "m4_mapall_sep"
                    "m4_map_args"
                    "m4_map_args_pair"
                    "m4_map_args_sep"
                    "m4_map_args_w"
                    "m4_shiftn"
                    "m4_shift2"
                    "m4_shift3"
                    "m4_stack_foreach"
                    "m4_stack_foreach_lifo"
                    "m4_stack_foreach_sep"
                    "m4_stack_foreach_sep_lifo"
                    "m4_apply"
                    "m4_count"
                    "m4_curry"
                    "m4_do"
                    "m4_dquote"
                    "m4_quote_elt"
                    "m4_echo"
                    "m4_expand"
                    "m4_ignore"
                    "m4_make_list"
                    "m4_quote"
                    "m4_reverse"
                    "m4_unquote"
                    "m4_append"
                    "m4_append_uniq"
                    "m4_append_uniq_w"
                    "m4_chomp"
                    "m4_chomp_all"
                    "m4_combine"
                    "m4_escape"
                    "m4_flatten"
                    "m4_join"
                    "m4_joinall"
                    "m4_newline"
                    "m4_normalize"
                    "m4_re_escape"
                    "m4_split"
                    "m4_strip"
                    "m4_text_box"
                    "m4_text_wrap"
                    "m4_tolower"
                    "m4_toupper"
                    "m4_cmp"
                    "m4_list_cmp"
                    "m4_max"
                    "m4_min"
                    "m4_sign"
                    "m4_version_compare"
                    "m4_version_prereq"
                    "m4_set_add"
                    "m4_set_add_all"
                    "m4_set_contains"
                    "m4_set_contents"
                    "m4_set_dump"
                    "m4_set_delete"
                    "m4_set_difference"
                    "m4_set_intersection"
                    "m4_set_union"
                    "m4_set_empty"
                    "m4_set_foreach"
                    "m4_set_list"
                    "m4_set_listc"
                    "m4_set_map"
                    "m4_set_map_sep"
                    "m4_set_remove"
                    "m4_set_size"
                    "m4_pattern_forbid"
                    "m4_pattern_allow")
                  'symbols)
     . font-lock-keyword-face))
  "Default `font-lock-keywords' for Autotest mode.")

(defvar autotest-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?# "<\n" table)
    (modify-syntax-entry ?\n ">#" table)
    (modify-syntax-entry ?[ "(]" table)
    (modify-syntax-entry ?] ")[" table)
    (modify-syntax-entry ?{  "." table)
    (modify-syntax-entry ?}  "." table)
    (modify-syntax-entry ?_  "_" table)
    (modify-syntax-entry ?*  "." table)
    (modify-syntax-entry ?\"  "." table)
    table)
  "Syntax table used while in `autotest-mode'.")

(defun autotest--quoted-p (pos)
  "Return non-nil if POS is inside a quoted string."
  (let ((quoted nil))
    (dolist (o (nth 9 (save-excursion (syntax-ppss pos))))
      (if (eq (char-after o) ?\[) (setq quoted t)))
    quoted))

(defconst autotest-syntax-propertize
  (syntax-propertize-rules
   ("#" (0 (when (autotest--quoted-p (match-beginning 0))
             (string-to-syntax "."))))))

;;;###autoload
(define-derived-mode autotest-mode prog-mode "Autotest"
  "A major mode to edit autotest files."
  (setq-local comment-start "#")
  (setq-local parse-sexp-ignore-comments t)
  (setq-local syntax-propertize-function autotest-syntax-propertize)
  (setq-local font-lock-defaults '(autotest-font-lock-keywords nil)))

;;;###autoload
(add-to-list 'auto-mode-alist (cons (purecopy "\\.at\\'") 'autotest-mode))

(provide 'autotest-mode)
