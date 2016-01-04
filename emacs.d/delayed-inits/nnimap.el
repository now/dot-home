(require 'gnus-sum)
(defun now-gnus-summary-mute-articles (n)
  (interactive "P")
  (let (articles root-articles)
    (dolist (article (gnus-summary-work-articles n))
      (unless (member article articles)
        (let ((parent (gnus-summary-article-parent article)))
          (while parent
            (setq article parent)
            (setq parent (gnus-summary-article-parent parent))))
        (push article root-articles)
        (if articles
            (nconc articles (gnus-summary-articles-in-thread article))
          (setq articles (gnus-summary-articles-in-thread article)))))
    (sort articles '<)
    (when (and articles
               (with-current-buffer (nnimap-buffer)
                 (and (nnimap-command "UID STORE %s +X-GM-LABELS (\\Muted)"
                                      (nnimap-article-ranges articles))
                      (nnimap-command "UID STORE %s -X-GM-LABELS (\\Inbox)"
                                      (nnimap-article-ranges articles)))))
      (apply 'gnus-summary-remove-process-mark articles)
      (dolist (article (nreverse root-articles))
        (gnus-summary-goto-subject article)
        (gnus-summary-kill-thread)))))

(define-key gnus-summary-mode-map "m" 'now-gnus-summary-mute-articles)
