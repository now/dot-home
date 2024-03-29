;;; xterm.el --- term/xterm customizations           -*- lexical-binding: t; -*-

;; Copyright © 2022  Nikolai Weibull

;; Author: Nikolai Weibull <now@disu.se>
;; Keywords: local

;;; Code:

(require 'term/xterm)

;;;###autoload
(defun term/now-xterm-init ()
  (setq
   xterm-standard-colors
   '(("black"          0 (  0   0   0))
     ("red"            1 (149  22  22))
     ("green"          2 ( 37 115  37))
     ("yellow"         3 (118  96  32))
     ("blue"           4 ( 47  90 155))
     ("magenta"        5 ( 96  47 128))
     ("cyan"           6 ( 86 148 168))
     ("white"          7 (192 192 192))
     ("brightblack"    8 ( 24  24  24))
     ("brightred"      9 (240  38  38))
     ("brightgreen"   10 (  0 144   0))
     ("brightyellow"  11 (240 165   0))
     ("brightblue"    12 ( 32 128 192))
     ("brightmagenta" 13 (147  55  99))
     ("brightcyan"    14 (128 176 192))
     ("brightwhite"   15 (246 246 246))))
  (set-terminal-parameter nil 'background-mode 'light))

(provide 'term/now-xterm)
;;; xterm.el ends here
