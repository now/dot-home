(setq nxml-sexp-element-flag t
 nxml-slash-auto-complete-flag t)
(define-abbrev-table 'nxml-mode-abbrev-table ()
  "Abbrev table in use in nXML mode buffers.")
(define-abbrev nxml-mode-abbrev-table "s" "" 'nxml-mode-skeleton-xsl-stylesheet :system t)
(define-abbrev nxml-mode-abbrev-table "t" "" 'nxml-mode-skeleton-xsl-template :system t)
(add-hook 'nxml-mode-hook
          (lambda ()
            (setq local-abbrev-table nxml-mode-abbrev-table)))
(add-hook 'nxml-mode-hook 'now-set-fill-column-to-79)
(add-hook 'nxml-mode-hook 'turn-off-auto-fill)
(define-skeleton nxml-mode-skeleton-xsl-stylesheet
  "Insert an XSL Stylesheet."
  ""
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" \n
  > "<xsl:stylesheet version=\"1.0\" xmlns:xsl=\"http://www.w3.org/1999/XSL/Transform\">" \n
  > "<xsl:output method=\"xml\" encoding=\"UTF-8\"/>" \n
  > _ \n
  "</xsl:stylesheet>" >)
(define-skeleton nxml-mode-skeleton-xsl-template
  "Insert an XSL Template."
  ""
  > "<xsl:template match=\"" _ "\">" \n
  > "</xsl:template>" >)
(defun now-nxml-complete-or-indent-for-tab-command ()
  "Try to perform nXML completion or, failing that, indent line or region."
  (interactive)
  (unless (run-hook-with-args-until-success 'nxml-completion-hook)
    (call-interactively 'indent-for-tab-command)))
(define-key nxml-mode-map "\t" 'now-nxml-complete-or-indent-for-tab-command)
