(require 'org-mu4e)

;; TODO Check how drafts are handled by mu4e/message-mode
(setq mu4e-attachment-dir "~/Downloads"
      mu4e-bookmarks `(,(make-mu4e-bookmark
                         :name "Incoming"
                         :query (concat
                                 "maildir:/.Disuse.INBOX OR "
                                 "maildir:/.Amesto.INBOX OR "
                                 "maildir:/.Amesto.Webmaster.INBOX ")
                         :key ?i)
                       ,(make-mu4e-bookmark
                         :name "Flagged"
                         :query "flag:flagged "
                         :key ?f)
                       ,(make-mu4e-bookmark
                         :name "Archived"
                         :query (concat
                                 "maildir:/.Disuse.Archive OR "
                                 "maildir:/.Amesto.Archive ")
                         :key ?a)
                       ,(make-mu4e-bookmark
                         :name "Sent"
                         :query (concat
                                 "maildir:/.Disuse.Sent OR "
                                 "maildir:/.Amesto.Sent ")
                         :key ?s)
                       ,(make-mu4e-bookmark
                         :name "Junk"
                         :query (concat
                                 "maildir:/.Disuse.Junk OR "
                                 "maildir:/.Amesto.Junk OR "
                                 "maildir:/.Amesto.Webmaster.Junk ")
                         :key ?j)
                       ,(make-mu4e-bookmark
                         :name "Trashed"
                         :query (concat
                                 "maildir:/.Disuse.Trash OR "
                                 "maildir:/.Amesto.Trash OR "
                                 "maildir:/.Amesto.Webmaster.Trash ")
                         :key ?t)
                       ,(make-mu4e-bookmark
                         :name "Drafts"
                         :query (concat
                                 "maildir:/.Disuse.Drafts OR "
                                 "maildir:/.Amesto.Drafts OR "
                                 "maildir:/.Amesto.Webmaster.Drafts ")
                         :key ?d))
      mu4e-change-filenames-when-moving t
      mu4e-compose-dont-reply-to-self t
      mu4e-compose-format-flowed t
      mu4e-context-policy 'pick-first
      mu4e-contexts `(,(make-mu4e-context
                        :name "Disu.se"
                        :match-func (lambda (msg)
                                      (when msg
                                        (or (mu4e-message-contact-field-matches
                                             msg '(:from :to)
                                             "^\\(?:\\(?:now\\|nikolai\\)@\\(?:bitwi\\|disu\\.se\\)\\|nikolai\\.weibull@\\(?:gmail\\|icloud\\)\\.com\\)$")
                                            (string-prefix-p "/.Disuse." (mu4e-message-field msg :maildir)))))
                        :vars '((mu4e-compose-signature . t)
                                (mu4e-drafts-folder . "/.Disuse.Drafts")
                                (mu4e-refile-folder . "/.Disuse.Archive")
                                (mu4e-sent-folder . "/.Disuse.Sent")
                                (mu4e-trash-folder . "/.Disuse.Trash")
                                (user-mail-address . "now@disu.se")))
                      ,(make-mu4e-context
                        :name "Amesto"
                        :match-func (lambda (msg)
                                      (when msg
                                        (or (mu4e-message-contact-field-matches
                                             msg ':from
                                             "^nikolai\\.weibull@amesto\\.se$")
                                            (mu4e-message-contact-field-matches
                                             msg '(:to :cc :bcc)
                                             "@amesto\.se$"))))
                        :vars '((mu4e-compose-signature .
                                 (concat "Nikolai Weibull\n"
                                         "Systems Developer\n"
                                         "Office: +46 31-360 98 39 | "
                                         "Amesto Translations"))
                                (mu4e-drafts-folder . "/.Amesto.Drafts")
                                (mu4e-refile-folder . "/.Amesto.Archive")
                                (mu4e-sent-folder . "/.Amesto.Sent")
                                (mu4e-trash-folder . "/.Amesto.Trash")
                                (user-mail-address . "nikolai.weibull@amesto.se")))
                      ,(make-mu4e-context
                        :name "Webmaster"
                        :match-func (lambda (msg)
                                      (when msg
                                        (mu4e-message-contact-field-matches
                                         msg '(:from :to)
                                         "^webmaster@amesto\\.com$")))
                        :vars '((mu4e-compose-signature .
                                 (concat "Nikolai Weibull\n"
                                         "Systems Developer\n"
                                         "Office: +46 31-360 98 39 | "
                                         "Amesto Translations"))
                                (mu4e-drafts-folder . "/.Amesto.Webmaster.Drafts")
                                (mu4e-refile-folder . "/.Amesto.Webmaster.Archive")
                                (mu4e-sent-folder . "/.Amesto.Webmaster.Sent")
                                (mu4e-trash-folder . "/.Amesto.Webmaster.Trash")
                                (user-mail-address . "webmaster@amesto.com"))))
      mu4e-headers-fields '((:flags . 4)
                            (:human-date . 11)
                            (:from-or-to . 22)
                            (:subject))
      mu4e-headers-fields '((:flags . 4)
                            (:subject . 127)
                            (:from-or-to . 22)
                            (:human-date . 11))
      mu4e-headers-from-or-to-prefix '("" . "« ")
      mu4e-headers-date-format "%Y-%m-%d" ; TODO Just use the default, once we
                                          ; have our own locale set up.
      mu4e-headers-include-related nil
      mu4e-headers-show-target nil
      ; mu4e-headers-skip-duplicates t
      mu4e-headers-visible-columns 82
      mu4e-hide-index-messages t
      mu4e-index-lazy-check t
      mu4e-split-view 'vertical
      mu4e-update-interval nil
      mu4e-use-fancy-chars t
      mu4e-user-agent-string nil
      mu4e-user-mail-address-list '("now@bitwi.se"
                                    "now@disu.se"
                                    "nikolai.weibull@gmail.com"
                                    "nikolai.weibull@icloud.com"
                                    "nikolai.weibull@amesto.se")
      mu4e-view-actions '(("capture message" . mu4e-action-capture-message)
                          ("show this thread" . mu4e-action-show-thread)
                          ("view in browser" . mu4e-action-view-in-browser))
      mu4e-view-fields '(:from
                         :signature
                         :subject
                         :date
                         :decryption
                         :attachments
                         :to
                         :cc
                         :tags
                         :maildir)
      mu4e-view-prefer-html t)

;      mu4e-headers-from-or-to-prefix

(setq mu4e-headers-flagged-mark '("F" . "⚑")
      mu4e-headers-new-mark '("" . "")
      mu4e-headers-attach-mark '("" . "")
      mu4e-headers-passed-mark '("P" . "»")
      mu4e-headers-replied-mark '("R" . "«")
      mu4e-headers-seen-mark '("" . "")
      mu4e-headers-trashed-mark '("T" . "☑")
      mu4e-headers-unread-mark '("u" . "•"))

(setf (alist-get 'delete mu4e-marks)
      (plist-put (alist-get 'delete mu4e-marks) :char '("D" . "☒"))
      (alist-get 'flag mu4e-marks)
      (plist-put (alist-get 'flag mu4e-marks) :char '("+" . "⚑"))
      (alist-get 'read mu4e-marks)
      (plist-put (alist-get 'read mu4e-marks) :char '("!" . "◦"))
      (alist-get 'trash mu4e-marks)
      (plist-put (alist-get 'trash mu4e-marks) :char '("d" . "☑"))
      (alist-get 'unflag mu4e-marks)
      (plist-put (alist-get 'unflag mu4e-marks) :char '("-" . "⚐"))
      (alist-get 'unread mu4e-marks)
      (plist-put (alist-get 'unread mu4e-marks) :char '("?" . "•"))
      (alist-get 'untrash mu4e-marks)
      (plist-put (alist-get 'untrash mu4e-marks) :char '("=" . "☐"))
      (alist-get 'action mu4e-marks)
      (plist-put (alist-get 'action mu4e-marks) :char '("a" . "⚙"))
      (alist-get 'something mu4e-marks)
      (plist-put (alist-get 'something mu4e-marks) :char '("*" . "★")))

(add-to-list 'mu4e-marks
             '(later
               :char ("l" . "⌘")
               :prompt "later"
               :dyn-target (lambda (target msg) (mu4e-get-refile-folder msg))
               :action (lambda (docid msg target)
                         (mu4e~proc-move docid
                                         (mu4e~mark-check-target target)
                                         "+F-N"))))
(mu4e~headers-defun-mark-for later)
(mu4e~view-defun-mark-for later)

(setq mu4e-headers-has-child-prefix '("" . "")
      mu4e-headers-empty-parent-prefix '("" . "")
      mu4e-headers-first-child-prefix '("" . "")
      mu4e-headers-duplicate-prefix '("=" . "≡")
      mu4e-headers-default-prefix '("" . ""))

(add-hook 'mu4e-headers-mode-hook 'now-mu4e-headers-mode-hook)

(defun now-mu4e-headers-mode-hook ()
  (setf (alist-get 'truncation fringe-indicator-alist) nil))

(add-hook 'mu4e-view-mode-hook 'now-mu4e-view-mode-hook)

(defun now-mu4e-view-mode-hook ()
  (setq word-wrap t)
  (setf (alist-get 'continuation fringe-indicator-alist) nil))

(eval-after-load 'message
  '(setq mu4e-compose-mode-abbrev-table (make-abbrev-table)))

(defun now-mu4e-headers-quit-buffer ()
  "Quit the mu4e-view buffer or the mu4e-headers buffer."
  (interactive)
  (let* ((b (mu4e-get-view-buffer))
         (w (and (buffer-live-p b) (get-buffer-window b))))
    (if (not (window-live-p w))
        (mu4e~headers-quit-buffer)
      (select-window w)
      (mu4e~view-quit-buffer))))

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

(setq org-mu4e-link-query-in-headers-mode nil)

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

(defun now-mu4e-fill-paragraph (&optional justify region)
  "Fill a multi-line paragraph, respecting `mu4e-compose-format-flowed'."
  (interactive (progn
		 (barf-if-buffer-read-only)
		 (list (if current-prefix-arg 'full) t)))
  (if mu4e-compose-format-flowed
    (let ((fill-column (point-max)))
      (fill-paragraph justify region))
    (fill-paragraph justify region)))

(define-key mu4e-compose-mode-map (kbd "C-c C-q") 'now-mu4e-fill-yanked-message)
(define-key mu4e-compose-mode-map (kbd "M-q") 'now-mu4e-fill-paragraph)

;; Commands:
;;    mu4e-update-mail-and-index
;;    mu4e-headers-query-prev
;;    mu4e-headers-query-next
;;    org-mu4e-store-and-capture
;; Variables:
;;    mu4e-maildir-shortcuts

;; TODO Local postfix should e-mail locally.
