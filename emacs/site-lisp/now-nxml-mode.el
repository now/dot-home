;;; now-nxml-mode.el --- nxml-mode customizations    -*- lexical-binding: t; -*-

;; Copyright © 2022  Nikolai Weibull

;; Author: Nikolai Weibull <now@disu.se>
;; Keywords: local

;;; Code:

(require 'nxml-mode)

;;;###autoload
(defun now-nxml-mode-init ()
  "Initialize extensions to `nxml-mode'."
  (add-hook 'nxml-mode-hook 'now-set-fill-column-to-80)
  (add-hook 'nxml-mode-hook 'turn-off-auto-fill)
  (define-abbrev
    nxml-mode-abbrev-table
    "s"
    ""
    'now-nxml-skeleton-xsl-stylesheet
    :system t)
  (define-abbrev
    nxml-mode-abbrev-table
    "t"
    ""
    'now-nxml-skeleton-xsl-template
    :system t))

;;;###autoload
(define-skeleton now-nxml-skeleton-xsl-stylesheet
  "Insert an XSL Stylesheet."
  nil
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" \n
  "<xsl:stylesheet version=\"1.0\" xmlns:xsl=\"http://www.w3.org/1999/XSL/Transform\">" \n
  "<xsl:output method=\"xml\" encoding=\"UTF-8\"/>" \n
  _ \n
  "</xsl:stylesheet>" >)

;;;###autoload
(define-skeleton now-nxml-skeleton-xsl-template
  "Insert an XSL Template."
  nil
  "<xsl:template match=\"" _ "\">" \n
  "</xsl:template>" >)

(provide 'now-nxml-mode)
;;; now-nxml-mode.el ends here
