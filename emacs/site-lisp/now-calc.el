(require 'calc)

(defmath circleSegmentArea (r n)
              (interactive 2 "csa")
              (let ((θ (* 180 (/ n (* 2 pi r)))))
                (- (* (/ θ 180) pi (^ r 2)) (* (sin θ) (cos θ) (^ r 2)))))

(provide 'now-calc)
