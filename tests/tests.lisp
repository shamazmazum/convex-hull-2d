(in-package :convex-hull-2d/tests)

(defun run-tests ()
  (explain! (run 'convex-hull)))

(def-suite convex-hull :description "The main test suite")

(in-suite convex-hull)

(test convexity
  (loop with state = (make-random-state t)
        repeat 10000
        for points = (loop repeat 500 collect
                           (ch:point (random 1d0 state)
                                     (random 1d0 state)))
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
  (loop with state = (make-random-state t)
        repeat 10000
        for points = (loop repeat 500 collect
                           (ch:point (random 1d0 state)
                                     (random 1d0 state)))
        for hull = (ch:convex-hull points)
        for trias = (ch:triangularize hull)
        for inside-points = (set-difference points (ch:convex-hull-points hull)) do
        (is-true
         (every
          (lambda (point)
            (ch:inside-convex-hull-p trias point))
          inside-points))))

(test not-members
  (loop with state = (make-random-state t)
        repeat 10000
        for points = (loop repeat 500 collect
                           (ch:point (random 1d0 state)
                                     (random 1d0 state)))
        for outside = (loop repeat 500 collect
                            (ch:point (+ 1.01 (random 1d0 state))
                                      (+      (random 1d0 state))))
        for trias = (ch:triangularize (ch:convex-hull points)) do
        (is-true
         (notany
          (lambda (point)
            (ch:inside-convex-hull-p trias point))
          outside))))

(test perimeter-and-surface
  (loop with state = (make-random-state t)
        repeat 2000
        for points = (loop repeat 20000 collect
                           (ch:point (random 1d0 state)
                                     (random 1d0 state)))
        for trias = (ch:triangularize (ch:convex-hull points)) do
        (is (< (/ (abs (- 4 (ch:convex-hull-perimeter trias))) 4) 0.05))
        (is (< (/ (abs (- 1 (ch:convex-hull-surface   trias))) 1) 0.05))))
