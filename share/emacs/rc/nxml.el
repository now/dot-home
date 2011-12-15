(setq nxml-slash-auto-complete-flag t)
(add-to-list 'auto-mode-alist '("\\.xsd\\'" . nxml-mode))
(eval-after-load "rng-loc"
  '(add-to-list 'rng-schema-locating-files "~/share/emacs/etc/schema/schemas.xml"))

(eval-after-load 'nxml-mode
  '(progn
     (define-abbrev-table 'nxml-mode-abbrev-table ()
       "Abbrev table in use in nXML mode buffers.")
     (define-abbrev nxml-mode-abbrev-table "xsls" "" 'nxml-mode-skeleton-xsl-stylesheet)
     (define-skeleton nxml-mode-skeleton-xsl-stylesheet
       "Insert a XSL Stylesheet."
       ""
       > "<?xml version=\"1.0\" encoding=\"utf-8\"?>" \n
       > "<xsl:stylesheet version=\"1.0\" xmlns:xsl=\"http://www.w3.org/1999/XSL/Transform\">" \n
       > "<xsl:output method=\"xml\" encoding=\"utf-8\"/>" \n
       \n
       > _ \n
       "</xsl:stylesheet>" >)))
