(require 'buff-menu-ext)
(require 'ivy)

;;;###autoload
(defun counsel-buffer-menu ()
  "Use buffer-menu to switch to another buffer."
  (interactive)
  (let (entries)
    (with-current-buffer (list-buffers-noselect)
      (Buffer-menu-map-entries
       (lambda (b _e)
         (push (cons (buffer-substring (line-beginning-position)
                                       (line-end-position))
                     b)
               entries))))
    (ivy-read "Switch to buffer: " (nreverse entries)
              :history 'counsel-buffer-menu-history
              :preselect (if (> (length entries) 1) 2 1)
              :action #'counsel-buffer-menu-visit-buffer
              :caller 'counsel-buffer-menu)))

(defun counsel-buffer-menu-visit-buffer (x)
  "Switch to buffer of candidate X."
  (switch-to-buffer (cdr x)))

(defun counsel-buffer-menu-visit-buffer-other-window (x)
  "Switch to buffer of candidate X in another window."
  (switch-to-buffer-other-window (cdr x)))

(defun counsel-buffer-menu-visit-buffer-menu (_)
  "Switch to buffer-menu buffer."
  (switch-to-buffer "*Buffer List*"))

(ivy-set-actions
 'counsel-buffer-menu
 '(("j" counsel-buffer-menu-visit-buffer-other-window "other window")
   ("v" counsel-buffer-menu-visit-buffer-menu "switch to buffer menu")))
