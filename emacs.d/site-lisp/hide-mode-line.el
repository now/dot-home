(defvar hide-mode-line-saved-mode-line-format nil)
(make-variable-buffer-local 'hide-mode-line-saved-mode-line-format)
; TODO: add a hook of some kind when setting mode-line-format.

(defvar hide-mode-line nil)
; TODO: add a hook to run hide-mode-line-update when setting hide-mode-line.
; [or just use M-x hide-mode-line for now]

(defcustom hide-mode-line-unaffected-by-minibuffer t
  "If non-nil, the presence of a minibuffer by itself does not
force the mode line to be shown."
  :group 'hide-mode-line
  :type  'boolean)

(defun there-is-only-one-frame ()
  "Returns non-nil if there is only one frame, nil otherwise."
  (let ((frames (frames-on-display-list)))
    (if (= (length frames) 1)
        (car frames)
      nil)))

(defun there-is-only-one-window-in (frame)
  "Returns non-nil if there is only one window in the specified FRAME,
nil otherwise."
  (let ((root (car (window-tree frame))))
    (not (listp root))))

(defun one-frame-and-one-window-p ()
  "Returns non-nil if there is only one frame and there is only
one window in that frame, nil otherwise."
  (let ((the-only-frame (there-is-only-one-frame)))
    (and the-only-frame
         (or hide-mode-line-unaffected-by-minibuffer
             (= (minibuffer-depth) 0))
         (there-is-only-one-window-in the-only-frame))))

(defun hide-mode-line-update ()
  "Update the state of all buffers' mode lines."
  (let* ((hide (and hide-mode-line (one-frame-and-one-window-p)))
         (variables '(mode-line-format hide-mode-line-saved-mode-line-format)))
    (when hide
      (setq variables (reverse variables)))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (if hide
                (not hide-mode-line-saved-mode-line-format)
                hide-mode-line-saved-mode-line-format)
          (set (car variables) (eval (cadr variables)))
          (set (cadr variables) nil))))))

(defun hide-mode-line-minibuffer-setup-hook ()
  (hide-mode-line-update))

(defun hide-mode-line-minibuffer-exit-hook ()
  (hide-mode-line-update))

(defun hide-mode-line-make-frame-function (new-frame)
  (hide-mode-line-update))

(defun hide-mode-line-delete-frame-function (dead-frame-walking)
  (hide-mode-line-update))

(defun hide-mode-line-window-configuration-change-hook ()
  (hide-mode-line-update))

(defun hide-mode-line-add-hooks ()
  (interactive)
  (add-hook 'minibuffer-setup-hook
            'hide-mode-line-minibuffer-setup-hook)
  (add-hook 'minibuffer-exit-hook
            'hide-mode-line-minibuffer-exit-hook)
  (add-hook 'after-make-frame-functions
            'hide-mode-line-make-frame-function)
  (add-hook 'delete-frame-functions
            'hide-mode-line-delete-frame-function)
  (add-hook 'window-configuration-change-hook
            'hide-mode-line-window-configuration-change-hook))

(defun hide-mode-line-remove-hooks ()
  (interactive)
  (remove-hook 'minibuffer-setup-hook
               'hide-mode-line-minibuffer-setup-hook)
  (remove-hook 'minibuffer-exit-hook
               'hide-mode-line-minibuffer-exit-hook)
  (remove-hook 'after-make-frame-functions
               'hide-mode-line-make-frame-function)
  (remove-hook 'delete-frame-functions
               'hide-mode-line-delete-frame-function)
  (remove-hook 'window-configuration-change-hook
               'hide-mode-line-window-configuration-change-hook))

;;;###autoload
(defun hide-mode-line-unhide-temporarily ()
  (interactive)
  (when (and hide-mode-line hide-mode-line-saved-mode-line-format)
    (setq mode-line-format hide-mode-line-saved-mode-line-format)
    (redraw-display)
    ;(force-mode-line-update)
    (sit-for 2)
    (setq mode-line-format nil)))

;;;###autoload
(defun hide-mode-line ()
  "Toggle the hide-mode-line functionality."
  (interactive)
  (setq hide-mode-line (not hide-mode-line))
  (hide-mode-line-update))

(hide-mode-line-add-hooks)

(provide 'hide-mode-line)
