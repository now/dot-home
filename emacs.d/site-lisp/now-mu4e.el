(require 'mu4e)

;;;###autoload
(defun now-mu4e-headers-quit-buffer ()
  "Quit the mu4e-view buffer or the mu4e-headers buffer."
  (interactive)
  (let* ((b (mu4e-get-view-buffer))
         (w (and (buffer-live-p b) (get-buffer-window b))))
    (if (not (window-live-p w))
        (mu4e~headers-quit-buffer)
      (select-window w)
      (mu4e~view-quit-buffer))))

;;;###autoload
(defun now-mu4e-view-scroll-down-or-prev ()
  "Scroll-down the current message.
If `mu4e-view-scroll-to-next' is non-nil, and we can't scroll-down
anymore, go to the previous message."
  (interactive)
  (condition-case nil
    (scroll-down)
    (error
      (when mu4e-view-scroll-to-next
	(mu4e-view-headers-prev)))))

;;;###autoload
(defun now-mu4e-fill-yanked-message (&optional justify)
  "Fill the paragraphs of a message yanked into this one.
Numeric argument means justify as well.  This function respects
`mu4e-compose-format-flowed'."
  (interactive (progn
		 (barf-if-buffer-read-only)
		 (list (if current-prefix-arg 'full))))
  (save-excursion
    (goto-char (point-min))
    (search-forward (concat "\n" mail-header-separator "\n") nil t)
    (let ((max (save-excursion
                 (or (re-search-forward message-signature-separator nil t)
                     (point-max)))))
      (if mu4e-compose-format-flowed
          (let ((fill-column max)
                (fill-prefix message-yank-prefix))
            (fill-individual-paragraphs (point) max justify))
        (let ((fill-prefix message-yank-prefix))
          (fill-individual-paragraphs (point) max justify))))))

;;;###autoload
(defun now-mu4e-fill-paragraph (&optional justify region)
  "Fill a multi-line paragraph, respecting `mu4e-compose-format-flowed'."
  (interactive (progn
		 (barf-if-buffer-read-only)
		 (list (if current-prefix-arg 'full) t)))
  (if mu4e-compose-format-flowed
    (let ((fill-column (point-max)))
      (fill-paragraph justify region))
    (fill-paragraph justify region)))

(provide 'now-mu4e)
