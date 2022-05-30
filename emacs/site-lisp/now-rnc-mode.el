;;; now-rnc-mode.el --- rnc-mode customizations      -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Nikolai Weibull

;; Author: Nikolai Weibull <now@disu.se>
;; Keywords: local

;;; Code:

(require 'rnc-mode)

;;;###autoload
(defun now-rnc-mode-init ()
  "Initialize customizations to `rnc-mode'."
  (define-abbrev
    rnc-mode-abbrev-table
    "a"
    ""
    'now-rnc-mode-skeleton-attribute
    :system t)
  (define-abbrev
    rnc-mode-abbrev-table
    "d"
    ""
    'now-rnc-mode-skeleton-div
    :system t)
  (define-abbrev
    rnc-mode-abbrev-table "e" "" 'now-rnc-mode-skeleton-element :system t))

;;;###autoload
(define-skeleton now-rnc-mode-skeleton-attribute
  "Insert an attribute definition."
  "Prefix: "
  "div {" \n
  str
  '(setq v1 (skeleton-read "Element name: "))
  '(setq v2 (skeleton-read "Attribute name: "))
  "." v1 ".attributes &= " str "." v1 ".attributes." v2 \n
  str "." v1 ".attributes." v2 " = attribute " v2 " { " _ " }" \n
  "}" >)

;;;###autoload
(define-skeleton now-rnc-mode-skeleton-div
  "Insert a div."
  nil
  "div {" \n
  _ \n
  "}" >)

;;;###autoload
(define-skeleton now-rnc-mode-skeleton-element
  "Insert an element definition."
  "Prefix: "
  "div {" \n
  str
  '(setq v1 (skeleton-read "Element name: "))
  "." v1 " = element " v1 " { " str "." v1 ".attributes, "
  str "." v1 ".content }" "\n"
  \n
  str "." v1 ".attributes = " _ "\n"
  \n
  (- rnc-indent-level) str "." v1 ".content = " \n
  "}" >)

(provide 'now-rnc-mode)
;;; now-rnc-mode.el ends here
