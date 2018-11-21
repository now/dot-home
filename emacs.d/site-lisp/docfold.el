(require 'cl-extra)

(defgroup docfold nil
  "Minor mode for hiding sections of code based on comments."
  :prefix "docfold-"
  :group 'languages)

;;;###autoload
(defcustom docfold-set-up-overlay 'docfold-set-up-overlay
  "Function called to set up newly created docfold overlays.
The function will be called with one argument, the newly created
overlay.  See `docfold-make-overlay' and info
node `(elisp)Overlays'."
  :type 'symbol
  :group 'docfold
  :risky t)

(defcustom docfold-ellipsis "…"
  "String to display at the end of docfold overlays.
This string is used by `docfold-set-up-overlay' and
`docfold-c-set-up-overlay', which set the \\='display property of
the overlay to be a substring of the part of the buffer covered
by the overlay."
  :type 'string
  :group 'docfold)

(defun docfold-set-up-overlay (overlay)
  "Default function called to set up docfold OVERLAY.
This function will set the \\='display property of the overlay to
the first sentence of the part of the buffer covered by the
overlay, followed by `docfold-ellipsis'."
  (save-excursion
    (goto-char (overlay-start overlay))
    (forward-sentence)
    (overlay-put overlay 'display
                 (concat (buffer-substring (overlay-start overlay) (point))
                         docfold-ellipsis))))

(defun docfold-c-set-up-overlay (overlay)
  "Function called to set up docfold OVERLAY for `c-mode'.
This function will set the \\='display property of the overlay to
the first first sentence of the comment of the part of the buffer
covered by the overlay, followed by `docfold-ellipsis', a
newline, and any code before the first `{' that follows the
comment, followed by `docfold-ellipsis'."
  (if (save-excursion
        (goto-char (overlay-start overlay))
        (looking-at-p "[ \t]"))
      (docfold-set-up-overlay overlay)
    (let ((comment (buffer-substring (overlay-start overlay)
                                     (save-excursion
                                       (goto-char (overlay-start overlay))
                                       (forward-sentence)
                                       (point)))))
      (save-excursion
        (goto-char (overlay-start overlay))
        (forward-comment (buffer-size))
        (skip-chars-forward " \t\n\f")
        (let ((p (point))
              (q (when (< (point) (overlay-end overlay))
                   (save-excursion
                     (save-match-data
                       (when (re-search-forward "{" (overlay-end overlay) t)
                         (skip-chars-backward "{ \t\n\f")
                         (point)))))))
          (if (or (>= p (overlay-end overlay)) (not q))
              (overlay-put overlay 'display (concat comment docfold-ellipsis))
            (overlay-put overlay 'display
                         (concat comment docfold-ellipsis "\n"
                                 (buffer-substring p q) docfold-ellipsis))))))))

(defun docfold-forward-section (&optional n target)
  "Move N docfold sections forward (backward if N is negative).
N must not be 0 and defaults to 1.  If `point' is already on a
section header, that counts as the first segment.  `Point' will
be placed on the first character of the Nth section header or at
the end (beginning) of the buffer, if those are reached before N
sections have been passed.

If TARGET is given, only sections with that many characters of
indent will be counted towards reaching N.

Returns the number of characters used as indent for the section
at `point'."
  (cond
   ((null n) (setq n 1))
   ((= n 0) (error "Must move at least one section forward or backward")))
  (beginning-of-line)
  (while (and (or (not (forward-comment 1))
                  (not (forward-comment -1))
                  (and target
                       (> (- (point) (line-beginning-position)) target))
                  (/= (setq n (+ n (if (> n 0) -1 1))) 0))
              (not (if (> n 0) (eobp) (bobp))))
    (forward-line (cl-signum n)))
  (when (and (save-excursion (forward-comment 1))
             (or (not target)
                 (<= (- (point) (line-beginning-position)) target)))
    (forward-comment (- (buffer-size)))
    (skip-chars-forward " \t\n\f")
    (- (point) (line-beginning-position))))

(defun docfold-backward-section (&optional n target)
  "Move N docfold sections backward (forward if N is negative).
N must not be 0 and defaults to -1.  If `point' is already on a
section header, that counts as the first segment.  `Point' will
be placed on the first character of the Nth section header or at
the end (beginning) of the buffer, if those are reached before N
sections have been passed.

If TARGET is given, only sections with that many characters of
indent will be counted towards reaching N.

Returns the number of characters used as indent for the section
at `point'."
  (docfold-forward-section (- (or n 1)) target))

;; TODO Why not use text properties instead?  Should be about the
;; same?  Though text properties doesn’t work with isearch.  And
;; nesting would require a bit more work (a counter that would record
;; nesting level and we would add and remove to it when
;; folding/unfolding).
(defun docfold-make-overlay (beginning end &optional indent position)
  "Return a new overlay spanning BEGINNING to END.
Actually, the overlay BEGINNING is adjusted to start at the
beginning of the line that BEGINNING belongs to.  When INDENT is
non-nil, END is likewise adjusted to end at the end of the first
line that precedes a line with less characters of indent than
INDENT.

The overlay isn’t created if there is only one line between
BEGINNING and END.

If POSITION is given, it is saved in the \\='docfold-point
property to later be used to restore `point' to when the overlay
is removed.

The overlay has the \\='invisible, \\='isearch-open-invisible,
\\='isearch-open-invisible-temporary, \\='docfold-point, and
\\='face properties set and `docfold-set-up-overlay' may set
further properties, such as \\='display."
  (let ((real-beginning (save-excursion (goto-char beginning)
                                        (line-beginning-position)))
        (real-end end))
    (when indent
      (setq real-end real-beginning)
      (save-excursion
        (goto-char real-end)
        (while (and (< real-end end)
                    (>= (skip-syntax-forward " " (line-end-position)) indent))
          (forward-line)
          (setq real-end (1- (point))))
        ;; TODO Can this happen?
        (when (> real-end end)
          (setq real-end end))))
    (when (> (count-lines real-beginning real-end) 1)
      (let ((o (make-overlay real-beginning real-end)))
        (overlay-put o 'invisible 'docfold)
        (overlay-put o 'isearch-open-invisible 'delete-overlay)
        (overlay-put o 'isearch-open-invisible-temporary
                     'docfold--isearch-open-invisible-temporary)
        (overlay-put o 'docfold-point position)
        (overlay-put o 'face 'default)
        (funcall docfold-set-up-overlay o)
        o))))

(defun docfold--isearch-open-invisible-temporary (overlay hide-p)
  "Temporarily display the content of OVERLAY.
This function is called by `isearch' to temporarily display the
current match.

If `hide-p' is nil, store the value of the \\='display property
of OVERLAY in the \\='docfold-isearch-display property and then
set the \\='display property and the \\='invisible property to
nil.  Otherwise, restore the \\='display property to the value
saved in \\='docfold-isearch-display and set the \\='invisible
property to \\='docfold."
  (if hide-p
      (let ((value (overlay-get overlay 'docfold-isearch-display)))
        (when value
          (overlay-put overlay 'display value)
          (overlay-put overlay 'docfold-isearch-display nil)))
    (let ((value (overlay-get overlay 'display)))
      (when value
        (overlay-put overlay 'docfold-isearch-display value)
        (overlay-put overlay 'display nil))))
  (overlay-put overlay 'invisible (when hide-p 'docfold)))

(defun docfold-overlay-at (position)
  "Return the first docfold overlay that contain the character at POSITION.
See `docfold-overlays-at' for more information."
  (car (docfold-overlays-at position)))

(defun docfold-overlays-at (position)
  "Return a list of the docfold overlays that contain the character at POSITION.
The overlays selected are those that have the \\='invisible
property set to \\='docfold."
  (let ((os (list)))
    (dolist (o (overlays-at position))
      (when (eq (overlay-get o 'invisible) 'docfold)
        (setq os (cons o os))))
    (nreverse os)))

(defun docfold-overlays-in (&optional beginning end)
  "Return a list of the docfold overlays that overlap BEGINNING..END.
See `overlays-in' for more information."
  (let ((os (list)))
    (dolist (o (overlays-in (or beginning (point-min)) (or end (point-max))))
      (when (eq (overlay-get o 'invisible) 'docfold)
        (setq os (cons o os))))
    os))

(defun docfold-hide-section ()
  "Hide the docfold section at `point'.
This is the same as `docfold-hide-section-recursively' with a LIMIT of 0."
  (interactive)
  (docfold-hide-section-recursively 0))

(defun docfold-hide-section-recursively (&optional limit)
  "Hide the docfold section at `point' recursively.
First, go `docfold-backward-section' to get to the section header
of the section that `point' is in.  If that section is already
hidden, go `docfold-backward-section' again.  Repeat this process
until we arrive at a section that isn’t hidden.  If we arrive at
a section header, hide it and any sections below it, stopping at
sub-section level LIMIT."
  (interactive "p")
  (let ((sp (point))
        (indent (let ((i (docfold-backward-section)))
                  (while (and i (> i 0) (docfold-overlay-at (point)))
                    (forward-comment (- (buffer-size)))
                    (setq i (docfold-backward-section)))
                  i))
        (p (point)))
    (when indent
      (let ((o (docfold-overlay-at p))) (when o (delete-overlay o)))
      (forward-comment (buffer-size))
      (docfold--hide-section-recursively indent indent limit)
      (docfold-make-overlay p (- (line-beginning-position) 1) indent sp)
      (goto-char p)
      (beginning-of-line))))

(defun docfold--hide-section-recursively (&optional base-indent indent limit)
  "Do the recursive work of `docfold-hide-section-recursively'.
This is done by checking if the indent of
`docfold-forward-section' is greater than the current INDENT.  If
so, fold all docfold sections at this greater level of indent,
that is, docfold sub-sections and also call this function
recursively inside those sections while limit is greater than 0."
  (unless base-indent (setq base-indent 0))
  (unless indent (setq indent base-indent))
  (unless limit (setq limit 1))
  (if (= limit 0)
      (docfold-forward-section 1 base-indent)
    (let ((inner-indent (docfold-forward-section))
          (p (point)))
      (when (and inner-indent (> inner-indent indent))
        (forward-comment (buffer-size))
        (while (let ((next-indent
                      (docfold-forward-section 1 inner-indent)))
                 (and next-indent (= next-indent inner-indent)))
          (save-excursion
            (docfold--hide-section-recursively base-indent inner-indent
                                               (1- limit)))
          (docfold-make-overlay p (- (line-beginning-position) 1) inner-indent)
          (setq p (point))
          (forward-comment (buffer-size)))
        (docfold-make-overlay p (- (line-beginning-position) 1) inner-indent)))))

(defun docfold-hide-all ()
  "Hide all top-level docfold sections and their sub-sections.
First, call `docfold-show-all' to show all docfold sections.
Then iterate through all top-level docfold sections using
`docfold-forward-section' and call
`docfold-hide-section-recursively' on them."
  (interactive)
  (docfold-show-all)
  (save-excursion
    (goto-char (point-min))
    (while (docfold-forward-section 1 0)
      (docfold-hide-section-recursively)
      (goto-char (1+ (overlay-end (docfold-overlay-at (point))))))
    (beginning-of-line)))

(defun docfold-show-section (&optional point-at-end)
  "Show the docfold section at `point'.

If POINT-AT-END is non-nil, place `point' at the end of the
docfold section.  Otherwise, place `point' at the position stored
in the \\='docfold-point property of the overlay in question."
  (interactive "P")
  (let* ((os (docfold-overlays-at (point)))
         (p (when (car os)
              (if point-at-end
                  (overlay-end (car os))
                (overlay-get (car os) 'docfold-point)))))
    (dolist (o os)
      (delete-overlay o))
    (when p
      (goto-char p))))

(defun docfold-show-section-recursively (&optional point-at-end)
  "Show the docfold section at `point' recursively.
First calls `docfold-show-section', then shows all sub-sections
inside of the docfold section."
  (interactive "P")
  (let ((o (docfold-overlay-at (point))))
    (when o
      (let ((beginning (overlay-start o))
            (end (overlay-end o)))
        (docfold-show-section point-at-end)
        (dolist (o (docfold-overlays-in beginning end))
          (delete-overlay o))))))

;; TODO Take region?
(defun docfold-show-all ()
  "Show all docfold sections."
  (interactive)
  (dolist (o (docfold-overlays-in))
    (delete-overlay o)))

(defun docfold-toggle-section (&optional point-at-end)
  "Toggle hiding and showing the docfold section at `point'.

See `docfold-show-section' for a description of how POINT-AT-END
is used."
  (interactive "P")
  (if (docfold-overlay-at (point))
      (docfold-show-section point-at-end)
    (docfold-hide-section)))

(defun docfold--occur-mode-find-occurence ()
  (docfold--next-error-move nil (point)))

(defun docfold--next-error-move (_error-buffer-position error-position)
  (save-excursion
    (goto-char error-position)
    (docfold-show-section)))

(defvar-local docfold--line-move-ignore-invisible nil
  "Variable to store the value of `line-move-ignore-invisible'
  before `docfold-minor-mode' was activated so that it may be
  restored afterwards.")

;; TODO Docstring.
;;;###autoload
(define-minor-mode docfold-minor-mode
  ""
  :group 'docfold
  :lighter " df"
  (if docfold-minor-mode
      (progn
        (setq docfold--line-move-ignore-invisible line-move-ignore-invisible)
        (setq-local line-move-ignore-invisible t)
        (add-hook 'occur-mode-find-occurrence-hook
                  'docfold--occur-mode-find-occurence nil t)
        (setq next-error-move-function 'docfold--next-error-move))
    (setq-local line-move-ignore-invisible docfold--line-move-ignore-invisible)
    (docfold-show-all)))

(provide 'docfold)
