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
      (add "ned")
      (load-rc t "os" (symbol-name window-system))
      (rc-progmode "cc-mode")
      (rc-progmode "compile")
      (rc-progmode "grep")
      (rc-progmode "ruby-mode")
      (rc "ui")
      (rc "desktop")
      (rc "diff")
      (rc "digraph")
      (rc "icomplete")
      (rc "ido")
      (rc "isearch")
      (rc "smex")
      (rc "viper")
      (rc "woman"))))

(require 'ned-info-on-file)

;(windmove-default-keybindings)

;; Indentation.
(setq-default indent-tabs-mode nil)

; TODO: Undo/Redo for window configurations
; (winner-mode t)

;; Might need to move these to c hook.
(setq-default fill-column 79)
(auto-fill-mode 1)

;; enable-recursive-minibuffers

;; TODO: This is probably uninteresting as we should implement Vim’s Ctrl-V
;; instead.
(setq read-quoted-char-radix 16)

(setq history-length 512
      make-backup-files nil) ; TODO: Not totally sure about disabling this.
