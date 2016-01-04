(require 'nnir)
(setq gnus-ancient-mark ?\s
      gnus-fetch-old-headers t
      gnus-parameters '((".*"
                         (gnus-use-scoring nil)
                         (display . all)))
      gnus-permanently-visible-groups "INBOX\'"
      gnus-select-method '(nnimap "gmail"
                                  (nnimap-address "imap.gmail.com")
                                  (nnimap-stream ssl))
      gnus-summary-line-format "%U%R%z%>%(%*%-23,23f%)  %s%-67=  %11&user-date;\n"
      gnus-thread-sort-functions '((not gnus-thread-sort-by-number))
      gnus-unread-mark ?\·
      gnus-use-corrent-string-widths t
      gnus-user-date-format-alist '(((gnus-seconds-today) . "%H:%M")
                                    ((+ 86400 (gnus-seconds-today)) . "%a %H:%M")
                                    (604800 . "%a, %b %-d")
                                    (15778476 . "%b %-d")
                                    (t . "%Y-%m-%d")))

(defun now-gnus-summary-mute-articles (n)
  (interactive "P")
  (let ((articles (list))
        thread-starters)
    (dolist (article (gnus-summary-work-articles n))
      (unless (member article articles)
        (let ((parent (gnus-summary-article-parent article)))
          (while parent
            (setq article parent)
            (setq parent (gun-summary-article-parent parent))))
        (push article thread-starters)
        (setq articles (append articles (gnus-summary-articles-in-thread article)))))
    (sort articles '<)
    (when (with-current-buffer (nnimap-buffer)
            (and (nnimap-command "UID STORE %s +X-GM-LABELS (\\Muted)"
                                 (nnimap-article-ranges articles))
                 (nnimap-command "UID STORE %s -X-GM-LABELS (\\Inbox)"
                                 (nnimap-article-ranges articles))))
      (apply 'gnus-summary-remove-process-mark articles)
      (dolist (article thread-starters)
        (gnus-summary-goto-subject article)
        (gnus-summary-kill-thread)))))

(define-key gnus-summary-mode-map "m" 'now-gnus-summary-mute-articles)
