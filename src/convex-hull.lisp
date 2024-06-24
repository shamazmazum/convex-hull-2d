(in-package :convex-hull-2d)

(sera:defconstructor point
  (x single-float)
  (y single-float))

(sera:defstruct-read-only
    (convex-hull (:constructor %convex-hull (points)))
  "A list of points belonging to the convex hull"
  (points nil :type list))

(sera:-> minus (point point)
         (values point &optional))
(declaim (inline minus))
(defun minus (p1 p2)
  "p1 - p2"
  (point (- (point-x p1) (point-x p2))
         (- (point-y p1) (point-y p2))))

(sera:-> plus (point point)
         (values point &optional))
(declaim (inline plus))
(defun plus (p1 p2)
  "p1 + p2"
  (point (+ (point-x p1) (point-x p2))
         (+ (point-y p1) (point-x p2))))

(sera:-> cross-product (point point)
         (values single-float &optional))
(defun cross-product (p1 p2)
  (declare (optimize (speed 3)))
  (let ((x1 (point-x p1))
        (y1 (point-y p1))
        (x2 (point-x p2))
        (y2 (point-y p2)))
    (- (* x1 y2) (* y1 x2))))

;; Or is it rightp? Do not want to think about it
(sera:-> leftp (point point point)
         (values boolean &optional))
(defun leftp (origin p1 p2)
  (declare (optimize (speed 3)))
  (let ((p1 (minus p1 origin))
        (p2 (minus p2 origin)))
    (declare (dynamic-extent p1 p2))
    (< (cross-product p1 p2) 0)))

(sera:-> convex-hull-sorted (list)
         (values convex-hull &optional))
(defun convex-hull-sorted (points)
  (declare (optimize (speed 3)))
  (labels ((accumulate-points (acc points)
             (let ((point (car points)))
               (cond
                 ;; No more points
                 ((null points) acc)
                 ;; We have less than 2 points in the hull. Add a
                 ;; point to the hull
                 ((null (cdr acc))
                  (accumulate-points (cons point acc) (cdr points)))
                 ;; A new point is on the same side, e.g. to the left
                 ;; from the line segment, constructed from the
                 ;; previously added two points.
                 ((leftp (second acc) (first acc) point)
                  (accumulate-points (cons point acc) (cdr points)))
                 ;; A new point is not on the same side and forms a
                 ;; concavity. Drop the previously added point and try
                 ;; again.
                 (t
                  (accumulate-points (cdr acc) points))))))
    (%convex-hull
     (accumulate-points nil points))))

(sera:-> find-starting-point (list)
         (values point &optional))
(defun find-starting-point (points)
  (declare (optimize (speed 3)))
  (flet ((accum (p1 p2)
           (declare (type point p1 p2))
           (if (< (point-y p1) (point-y p2))
               p1 p2)))
    (reduce #'accum points)))

(sera:-> sort-points (list point)
         (values list &optional))
(defun sort-points (points start)
  "Sort points from the smallest angle between a segment Start-Point
and the axis X to the largest."
  (declare (optimize (speed 3)))
  (flet ((predicate (p1 p2)
           (let* ((p1 (minus p1 start))
                  (p2 (minus p2 start))
                  (x1 (point-x p1))
                  (x2 (point-x p2)))
             (or
              (and (>= x2 0) (<= x1 0))
              (let ((s1 (/ (point-y p1) x1))
                    (s2 (/ (point-y p2) x2)))
                (or
                 (and (< s2 s1) (< x1 0))
                 (and (> s1 s2) (> x2 0))))))))
    (sb-int:with-float-traps-masked (:divide-by-zero)
      (cons start (sort (remove start (copy-seq points)) #'predicate)))))

(sera:-> convex-hull (list)
         (values convex-hull &optional))
(defun convex-hull (points)
  "Calculate the convex hull of a set of points. POINTS must be a list
of objects with the type POINT."
  (let ((start (find-starting-point points)))
    (convex-hull-sorted
     (sort-points points start))))

(sera:defconstructor triangle
  (p1 point)
  (p2 point)
  (p3 point))

(sera:defstruct-read-only
    (triangles (:constructor triangles (list)))
  "Triangularized convex hull"
  (list nil :type list))

(sera:-> average (list)
         (values point &optional))
(defun average (points)
  (declare (optimize (speed 3)))
  (let* ((length (length points))
         (sum (reduce #'plus points)))
    (point (/ (point-x sum) length)
           (/ (point-y sum) length))))

(sera:-> triangularize (convex-hull)
         (values triangles &optional))
(defun triangularize (convex-hull)
  "Triangularize a convex hull."
  (declare (optimize (speed 3)))
  (let* ((points (convex-hull-points convex-hull))
         (average (average points)))
    (triangles
     (maplist
      (lambda (ps)
        (let ((p1 (first ps))
              (p2 (or (second ps) (first points))))
          (triangle p1 p2 average)))
      points))))

(sera:-> inside-triangle-p (triangle point)
         (values boolean &optional))
(defun inside-triangle-p (triangle point)
  "Test if a point is inside a triangle."
  (declare (optimize (speed 3)))
  (let ((p1 (triangle-p1 triangle))
        (p2 (triangle-p2 triangle))
        (p3 (triangle-p3 triangle)))
    ;; A point must be on the same side from all the edges
    (not (or (leftp p1 p2 point)
             (leftp p2 p3 point)
             (leftp p3 p1 point)))))

(sera:-> inside-convex-hull-p (triangles point)
         (values boolean &optional))
(defun inside-convex-hull-p (triangles point)
  "Test if a point is inside a convex hull. The convex hull must be
triangularized with TRIANGULARIZE before being used in this function."
  (declare (optimize (speed 3)))
  (some (lambda (tri) (inside-triangle-p tri point))
        (triangles-list triangles)))

;; Perimeter & surface

(sera:-> norm (point)
         (values (single-float 0.0) &optional))
(declaim (inline norm))
(defun norm (point)
  (let ((x (point-x point))
        (y (point-y point)))
    (sqrt (+ (expt x 2) (expt y 2)))))

(sera:-> distance (point point)
         (values (single-float 0.0) &optional))
(declaim (inline distance))
(defun distance (p1 p2)
  (let ((p (minus p1 p2)))
    (declare (dynamic-extent p))
    (norm p)))

;; Not the perimeter of this triangle, but its contribution to the
;; perimeter of the convex hull.
(sera:-> triangle-perimeter (triangle)
         (values (single-float 0.0) &optional))
(defun triangle-perimeter (triangle)
  (declare (optimize (speed 3)))
  (distance (triangle-p1 triangle)
            (triangle-p2 triangle)))

(sera:-> triangle-surface (triangle)
         (values (single-float 0.0) &optional))
(defun triangle-surface (triangle)
  (declare (optimize (speed 3)))
  (let* ((p1 (triangle-p1 triangle))
         (p2 (triangle-p2 triangle))
         (p3 (triangle-p3 triangle))
         (d1 (distance p1 p2))
         (d2 (distance p2 p3))
         (d3 (distance p3 p1))
         (p (/ (+ d1 d2 d3) 2)))
    (sqrt (* p (- p d1) (- p d2) (- p d3)))))

(sera:-> convex-hull-perimeter (triangles)
         (values (single-float 0.0) &optional))
(defun convex-hull-perimeter (triangles)
  "Perimeter of the convex hull"
  (declare (optimize (speed 3)))
  (reduce #'+ (triangles-list triangles)
          :key #'triangle-perimeter))

(sera:-> convex-hull-surface (triangles)
         (values (single-float 0.0) &optional))
(defun convex-hull-surface (triangles)
  "Surface of the convex hull"
  (declare (optimize (speed 3)))
  (reduce #'+ (triangles-list triangles)
          :key #'triangle-surface))
