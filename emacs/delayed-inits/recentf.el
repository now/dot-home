(setq recentf-max-saved-items 50)

(defun ido-find-recent-file ()
  "Use `ido-completing-read' to `find-file' a recentf file"
  (interactive)
  (find-file (ido-completing-read "Find recent file: " recentf-list)))

(global-set-key (kbd "C-x C-r") 'ido-find-recent-file)
