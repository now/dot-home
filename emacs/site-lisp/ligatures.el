;;; ligatures.el --- ligatures                       -*- lexical-binding: t; -*-

;; Copyright © 2022  Nikolai Weibull

;; Author: Nikolai Weibull <now@disu.se>
;; Keywords: faces

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Ligatures.

;;; Code:

(defvar ligatures-alist
  `((t
     "Fl" "Tl" "fi" "fj" "www"
     (?* ,(rx ?* lower-case))
     (?+ ,(rx lower-case ?+ lower-case) 1)
     (?- ,(rx lower-case ?- lower-case) 1)
     (?: ,(rx upper-case ?: upper-case) 1)
     (?x ,(rx (or (sequence ?0 ?x hex) (sequence (any (?1 . ?9)) ?x digit))) 1))
    (emacs-lisp-mode
     "<=" ">=" "###")
    (java-mode
     "==" "<=" "/=" "&&" "/*" "//" "++" "<<" "<<<" "->" "::" "<!--"
     "!=" ">=" "^=" "||" "*/" "/>" "--" ">>" ">>>" "</" "<>" "-->")))

;;;###autoload
(define-minor-mode ligatures-mode "Enable typographic ligatures."
  :group 'faces
  (if (not ligatures-mode)
      (let ((parent (char-table-parent composition-function-table)))
        (unless (eq parent composition-function-table)
          (setq-local composition-function-table parent)))
    (let ((characters (make-char-table nil)))
      (dolist (specification ligatures-alist)
        (let ((modes (car specification))
              (ligatures (cdr specification)))
          (when (or
                 (eq modes t)
                 (if (listp modes)
                     (cl-some #'derived-mode-p modes)
                   (derived-mode-p modes)))
            (dolist (ligature ligatures)
              (cond
               ((and (stringp ligature) (length> ligature 1))
                (let* ((char (aref ligature 0))
                       (value (char-table-range characters char)))
                  (if (or (null value) (and (consp value) (stringp (car value))))
                      (set-char-table-range
                       characters char (cons ligature value))
                    (set-char-table-range characters char `(,ligature)))))
               ((and (listp ligature) (<= 2 (length ligature) 3))
                (set-char-table-range
                 characters (car ligature) `(ligature ,@(cdr ligature))))
               (t (error "unknown ligature: %S" ligature)))))))
      (let ((ligatures (make-char-table nil)))
        (map-char-table
         (lambda (char value)
           (set-char-table-range
            ligatures
            char
            `([,@(pcase value
                   (`(ligature ,pattern) `(,pattern 0))
                   (`(ligature . ,rest) rest)
                   (_
                    (let ((regexp (regexp-opt value t)))
                      `(,(substring regexp 2 (- (length regexp) 2)) 0))))
               font-shape-gstring])))
         characters)
        (set-char-table-parent ligatures composition-function-table)
        (setq-local composition-function-table ligatures)))))

(provide 'ligatures)
;;; ligatures.el ends here
