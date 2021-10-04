(defgroup highlight-selected-window nil
  "Highlight the `selected-window'."
  :group 'tools)

(defface highlight-selected-window-unselected-window
  '((((class color) (min-colors 256)) (:background "grey94" :extend t))
    (((class color)) (:background "grey" :extend t))
    (t (:inverse-video t)))
  "Face to remap the default face with in unselected windows.")

;; (defvar highlight-selected-window--face-remapped-buffers nil
;;   "List of buffers that have had their default face remapped.")

(defun highlight-selected-window--pre-redisplay-function (window)
  (unless (minibufferp)
    (if (eq (current-buffer) (window-buffer (selected-window)))
        (face-remap-reset-base 'default)
      (face-remap-set-base
       'default
       (flatten-list
        (face-all-attributes 'highlight-selected-window-unselected-window
                             (selected-frame)))))))

;; (dolist (buffer highlight-selected-window--face-remapped-buffers)
;;   (when (buffer-live-p buffer)
;;     (with-current-buffer buffer
;;       (face-remap-reset-base 'default))))
;; (setq highlight-selected-window--face-remapped-buffers nil)
;;   ;(mapc 'delete-overlay hide-mode-line-overlays)
;;   ;(setq hide-mode-line-overlays nil)

;; (dolist (window (window-list))
;;   (unless (or (eq window (minibuffer-window))
;;               (eq window (selected-window))
;;               (eq (window-buffer window) (window-buffer (selected-window))))
;;     (with-current-buffer (window-buffer window)
;;       (setq highlight-selected-window--face-remapped-buffers
;;             (cons (current-buffer)
;;                   highlight-selected-window--face-remapped-buffers))
;;       (face-remap-set-base 'default
;;                            'highlight-selected-window-unselected-window))))
;;         ;; (setq hide-mode-line-overlays
;;         ;;       (cons
;;         ;;        (make-overlay (point-min) (point-max) (current-buffer) nil t)
;;         ;;        hide-mode-line-overlays))
;;         ;; (overlay-put (car hide-mode-line-overlays) 'window window)
;;         ;; (overlay-put (car hide-mode-line-overlays)
;;         ;;              'face
;;         ;;              'hide-mode-line-overlay-face)
;;         ;; (overlay-put (car hide-mode-line-overlays)
;;         ;;              'after-string
;;         ;;              (propertize (make-string 200 ?\n) 'face
;;         ;;                          'hide-mode-line-overlay-face))))))

;(add-hook 'buffer-list-update-hook 'hide-mode-line-update2)
;(add-hook 'window-configuration-change-hook 'hide-mode-line-update2)

;;;###autoload
(define-minor-mode highlight-selected-window-mode
  "Highlight the `selected-window'.
This is actualy done by de-highlighting all other windows, remapping
the default face in those buffers with
`highlight-selected-window-unselected-window."
  :global t
  (apply (if highlight-selected-window-mode 'add-hook 'remove-hook)
      'pre-redisplay-functions
      '(highlight-selected-window--pre-redisplay-function))
  (unless highlight-selected-window-mode
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (face-remap-reset-base 'default)))))

(provide 'highlight-selected-window)
