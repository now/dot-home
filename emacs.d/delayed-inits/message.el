(require 'smtpmail-multi)
(setq message-citation-line-format "%N, %Y-%m-%d %R:\n"
      message-citation-line-function 'message-insert-formatted-citation-line
      message-kill-buffer-on-exit t
      message-send-mail-function 'smtpmail-multi-send-it)
;; TODO We need to run Alt-q on quoted text so that it wraps nicely.
;; Do this through message-indent-citation

;; Use widen to show more headers while editing.
;; C-c C-f C-c, C-c C-f C-i, C-c C-f C-a, C-c C-b, C-c C-i, C-c C-a,
;; C-c C-z, C-C C-v, M-RET, C-c C-d
;; ispell-message-dictionary-alist
;; TODO message-subscribed-addresses?

(define-abbrev message-mode-abbrev-table "br" "Best regards,\n  Nikolai"
  :system t :case-fixed t)

(add-hook 'message-mode-hook 'now-message-mode-hook)

(defun now-message-mode-hook ()
  (setf (alist-get 'continuation fringe-indicator-alist) nil)
  (flyspell-mode)
  (visual-fill-column-mode)
  (whitespace-mode -1))
