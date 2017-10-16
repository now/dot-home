(defvar hide-mode-line-saved-mode-line-format nil)
(make-variable-buffer-local 'hide-mode-line-saved-mode-line-format)
; TODO: add a hook of some kind when setting mode-line-format.

(defvar hide-mode-line nil)
; TODO: add a hook to run hide-mode-line-update when setting hide-mode-line.
; [or just use M-x hide-mode-line for now]

(defcustom hide-mode-line-unaffected-by-minibuffer t
  "If non-nil, the presence of a minibuffer by itself doesn’t
force the mode line to be shown."
  :group 'hide-mode-line
  :type  'boolean)

(defun hide-mode-line-update ()
  "Update the state of all buffers’ mode lines."
  (let ((hide (and hide-mode-line
                   (let ((frame (when (= (length (frames-on-display-list)) 1)
                                   (car (frames-on-display-list)))))
                     (and frame
                          (or hide-mode-line-unaffected-by-minibuffer
                              (= (minibuffer-depth) 0))
                          (not (listp (car (window-tree frame)))))))))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (if hide
                  (not hide-mode-line-saved-mode-line-format)
                hide-mode-line-saved-mode-line-format)
          (cond
           (hide (setq hide-mode-line-saved-mode-line-format mode-line-format)
                 (setq mode-line-format nil))
           (t (setq mode-line-format hide-mode-line-saved-mode-line-format)
              (setq hide-mode-line-saved-mode-line-format nil))))))))

(defun hide-mode-line-update-function (arg)
  (hide-mode-line-update))

;;;###autoload
(defun hide-mode-line-unhide-temporarily ()
  (interactive)
  (when (and hide-mode-line hide-mode-line-saved-mode-line-format)
    (message "%s" (format-mode-line hide-mode-line-saved-mode-line-format))))

;;;###autoload
(defun hide-mode-line ()
  "Toggle the hide-mode-line functionality."
  (interactive)
  (setq hide-mode-line (not hide-mode-line))
  (hide-mode-line-update))

(add-hook 'minibuffer-setup-hook 'hide-mode-line-update)
(add-hook 'minibuffer-exit-hook 'hide-mode-line-update)
(add-hook 'after-make-frame-functions 'hide-mode-line-update-function)
(add-hook 'delete-frame-functions 'hide-mode-line-update-function)
(add-hook 'window-configuration-change-hook 'hide-mode-line-update)

(provide 'hide-mode-line)
