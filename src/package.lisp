(defpackage convex-hull-2d
  (:use #:cl)
  (:local-nicknames (#:sera #:serapeum))
  (:export #:point
           #:point-x
           #:point-y
           #:convex-hull
           #:convex-hull-points
           #:triangle
           #:triangles
           #:triangles-list
           #:triangularize
           #:inside-triangle-p
           #:inside-convex-hull-p
           ;; For tests
           #:leftp))
