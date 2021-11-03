;;; typescriptreact-mode.el --- TypeScript with React language mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Nikolai Weibull

;; Author: Nikolai Weibull <now@now.pagero.local>
;; Keywords: languages

;;; Code
(require 'typescript-mode)

;;;###autoload
(define-derived-mode typescriptreact-mode typescript-mode "typescriptreact"
  "A major mode to edit TypeScript with React files.")

;;;###autoload
(add-to-list 'auto-mode-alist
             (cons (purecopy "\\.tsx\\'") 'typescriptreact-mode))

(provide 'typescriptreact-mode)
