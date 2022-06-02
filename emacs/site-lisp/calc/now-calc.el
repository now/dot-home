;;; now-calc.el --- calc customizations              -*- lexical-binding: t; -*-

;; Copyright © 2022  Nikolai Weibull

;; Author: Nikolai Weibull <now@disu.se>
;; Keywords: local

;;; Code:

(require 'calc)

;;;###autoload
(defun now-calc-init ()
  (setq
   calc-group-char " "
   calc-gnuplot-default-device "dumb"
   calc-show-banner nil))

(defmath circleSegmentArea (r n)
              (interactive 2 "csa")
              (let ((θ (* 180 (/ n (* 2 pi r)))))
                (- (* (/ θ 180) pi (^ r 2)) (* (sin θ) (cos θ) (^ r 2)))))

(provide 'now-calc)
;;; now-calc.el ends here
