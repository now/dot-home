(require 'rx)


              ;; (rx (: (* ?\s)
              ;;        (? (group (: (| ?\•
              ;;                        (+ (in (?₀ . ?₉))))
              ;;                     (* ?\s))))))

(defun nmc-adaptive-fill ()
  (when (looking-at " *= ")
    (concat (match-string 0) "  ")))

(defun nmc-fill-paragraph (justify)
  (save-excursion
    (move-to-left-margin)
    (if (not (zerop (fill-forward-paragraph 1)))
        ""
      (let ((end (point))
            (beg (progn (fill-forward-paragraph -1) (point))))
        (if use-hard-newlines
            (fill-region beg end justify)
          (skip-chars-forward " \t\n")
          (beginning-of-line)
          (fill-region-as-paragraph beg end justify nil
                                    (when (looking-at paragraph-start)
                                      (match-end 0))))))))

;; TODO Write a function that parses the current line up to point.
;; TODO Don’t break inside = …. =.  I think we need a way to fold
;; lines in nmc.
(defun nmc-fill-nobreak-p ()
  (save-excursion
    (let ((p (point)))
      (when (search-backward "‹" (line-beginning-position) t)
        (or (not (search-forward "›" (line-end-position) t))
            (< p (point)))))))

(defun nmc-backward-list-item ()
  (interactive)
  ;; See that we’re on a list item and record level (indent).  Then
  ;; move backward until we find a list-item with the same level.
  ;; Stop if we reach a non-empty line with a lesser level or that is
  ;; of another kind than a list item.
  )

(defun nmc-forward-list-item ()
  (interactive)
  )

(defun nmc-renumber-list ()
  (interactive)
  (beginning-of-line)
  (when (looking-at "\\( *\\)[₀-₉]")
    ))

;;;###autoload
(define-derived-mode nmc-mode text-mode "NMC"
  "Major mode for editing NoMarks Compact files.
\\{nmc-mode-map}"
  (setq-local adaptive-fill-function 'nmc-adaptive-fill)
  (setq-local adaptive-fill-regexp
              " *\\(\\(?:•\\|[₀-₉]+\\|= \\) *\\)?")
  (setq-local comment-start nil)
  (setq-local comment-start-skip nil)
  (setq-local fill-nobreak-predicate 'nmc-fill-nobreak-p)
  (setq-local fill-paragraph-function 'nmc-fill-paragraph)
  (setq-local fill-paragraph-handle-comment nil)
  (setq-local outline-regexp " *§")
  (setq-local paragraph-start (rx (| ?\f
                                     (: (* ?\s)
                                        (| eol
                                           (:
                                            (| ?§
                                               ?•
                                               (+ (in (?₀ . ?₉)))
                                               (: "= "))
                                               (* ?\s)))))))
  (auto-fill-mode)
  (outline-minor-mode))

;;;###autoload
(add-to-list 'auto-mode-alist (cons (purecopy "\\.nmc\\'") 'nmc-mode))

(provide 'nmc-mode)
