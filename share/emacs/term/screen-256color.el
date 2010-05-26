(load "term/xterm")
(defun terminal-init-screen ()
  "Terminal initialization function for screen."
  (xterm-register-default-colors)
  (tty-set-up-initial-frame-faces))
