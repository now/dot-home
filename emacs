;; -*- mode: emacs-lisp; coding: utf-8 -*-

(require 'cl)

(labels ((build-path (&rest components)
                     (let ((directories (mapcar #'file-name-as-directory (butlast components)))
                           (file (car (last components))))
                       (concat (apply #'concat directories) file))))
  (let ((my-share-emacs-path (build-path (expand-file-name "~") "share" "emacs")))
    (setq-default custom-theme-directory (build-path my-share-emacs-path "themes"))
    (add-to-list 'load-path my-share-emacs-path)
    (labels ((add (path) (add-to-list 'load-path (build-path my-share-emacs-path path)))
             (load-rc (missing-ok &rest components)
                      (load (apply #'build-path my-share-emacs-path "rc" components) missing-ok))
             (rc (&rest components) (apply #'load-rc nil components))
             (rc-progmode (mode) (rc "progmodes" mode)))
      (add "evil")
      (add "evil/lib")
      (add "magit")
      (add "ned")
      (add "yasnippet-0.6.1c")
      (load-rc t "os" (symbol-name window-system))
      (rc-progmode "cc-mode")
      (rc-progmode "compile")
      (rc-progmode "grep")
      (rc-progmode "ruby-mode")
      (rc "buffer")
      (rc "coding")
      (rc "custom")
      (rc "desktop")
      (rc "diff")
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
      (rc "minibuffer")
      (rc "nxml")
      (rc "org")
      (rc "paren")
      (rc "scroll-bar")
      (rc "simple")
      (rc "smex")
      (rc "startup")
      (rc "tool-bar")
      (rc "ui")
      (rc "uniquify")
      (rc "woman")
      (rc "xdisp")
      (rc "yasnippet"))))

(require 'ned-info-on-file)
