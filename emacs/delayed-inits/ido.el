(ido-everywhere 1)
(setq ido-auto-merge-work-directories-length -1
      ido-decorations (append '("\n" "" "\n" "\n…") (nthcdr 4 ido-decorations))
      ido-enable-flex-matching t
      ido-enable-last-directory-history nil
      ido-max-prospects 70
      ido-max-window-height 0.99
      ido-use-filename-at-point nil)
(defun ido-disable-line-truncation () (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)
