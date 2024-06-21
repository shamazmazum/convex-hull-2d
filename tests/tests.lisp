(in-package :convex-hull-2d/tests)

(defun run-tests ()
  (explain! (run 'convex-hull)))

(def-suite convex-hull :description "The main test suite")

(in-suite convex-hull)

(test convexity
  (loop repeat 100 
        for points = (loop repeat 500 collect
                           (ch:point (random 1.0)
                                     (random 1.0)))
        for ch-points = (ch:convex-hull-points (ch:convex-hull points)) do
        (is-true
         (every
          (lambda (point)
            (or (find point ch-points)
                (notany
                 #'identity
                 (maplist
                  (lambda (ps)
                    (let ((p1 (first ps))
                          (p2 (or (second ps) (first ch-points))))
                      (ch:leftp p1 p2 point)))
                  ch-points))))
          points))))

(test members
  (loop repeat 100 
        for points = (loop repeat 500 collect
                           (ch:point (random 1.0)
                                     (random 1.0)))
        for hull = (ch:convex-hull points)
        for trias = (ch:triangularize hull)
        for inside-points = (set-difference points (ch:convex-hull-points hull)) do
        (is-true
         (every
          (lambda (point)
            (ch:inside-convex-hull-p trias point))
          inside-points))))

(test not-members
  (loop repeat 100 
        for points = (loop repeat 500 collect
                           (ch:point (random 1.0)
                                     (random 1.0)))
        for outside = (loop repeat 500 collect
                            (ch:point (+ 1.01 (random 1.0))
                                      (+      (random 1.0))))
        for trias = (ch:triangularize (ch:convex-hull points)) do
        (is-true
         (notany
          (lambda (point)
            (ch:inside-convex-hull-p trias point))
          outside))))
