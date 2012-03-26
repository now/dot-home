; TODO: Remove once we switch to Emacs 24
(eval-when-compile
  (require 'cl))

(defun now-do-not-show-trailing-whitespace ()
  (set (make-local-variable 'show-trailing-whitespace) nil))

(defun close-buffer-and-window-unless-last ()
  (interactive)
  (let* ((buffer (current-buffer))
         (window (get-buffer-window buffer))
         (next (next-window window)))
    (kill-buffer buffer)
    (when (and window
               (not (eq window next)))
      (delete-window window))))

(labels ((build-path (&rest components)
                     (let ((directories (mapcar #'file-name-as-directory (butlast components)))
                           (file (car (last components))))
                       (concat (apply #'concat directories) file))))
  (let ((my-share-emacs-path (build-path (expand-file-name "~") "share" "emacs" "lisp")))
    (setq-default custom-theme-directory (build-path my-share-emacs-path "themes"))
    (add-to-list 'load-path my-share-emacs-path)
    (require 'userloaddefs)
    (labels ((add (path) (add-to-list 'load-path (build-path my-share-emacs-path path)))
             (load-rc (missing-ok &rest components)
                      (load (apply #'build-path my-share-emacs-path "rc" components) missing-ok))
             (rc (&rest components) (apply #'load-rc nil components))
             (rc-progmode (mode) (rc "progmodes" mode))
             (rc-textmode (mode) (rc "textmodes" mode))
             (rc-vc (mode) (rc "vc" mode)))
      (add "evil")
      (add "evil/lib")
      (add "magit")
      (add "ned")
      (add "nxhtml/util")
      (add "progmodes")
      (load-rc t "os" (symbol-name system-type))
      (load-rc t "ws" (symbol-name window-system))
      (rc-progmode "cc-mode")
      (rc-progmode "compile")
      (rc-progmode "grep")
      (rc-progmode "hideshow")
      (rc-progmode "make-mode")
      (rc-progmode "rnc-mode")
      (rc-progmode "ruby-mode")
      (rc-progmode "sh-script")
      (rc-textmode "css-mode")
      (rc "bs")
      (rc "buffer")
      (rc "coding")
      (rc "custom")
      (rc "desktop")
      (rc "dired")
      (rc "disp-table")
      (rc "evil")
      (rc "files")
      (rc "frame")
      (rc "hide-mode-line")
      (rc "icomplete")
      (rc "ido")
      (rc "indent")
      (rc "info")
      (rc "isearch")
      (rc "magit")
      (rc "man")
      (rc "minibuffer")
      (rc "mumamo")
      (rc "nxml")
      (rc "org")
      (rc "paren")
      (rc "recentf")
      (rc "scroll-bar")
      (rc "simple")
      (rc "smex")
      (rc "startup")
      (rc "tool-bar")
      (rc "uniquify")
      (rc "vc")
      (rc-vc "diff")
      (rc "window")
      (rc "xdisp"))))
