convex-hull-2d
=============
This library is for calculation of the convex hull for a set of two-dimensional
points.

Convex hulls
------------

Let's create a set of random points:

``` lisp
(defparameter *points*
  (loop repeat 30 collect (convex-hull-2d:point (random 1.0) (random 1.0))))
```

Its convex hull can be computed as:

``` lisp
(defparameter *hull* (convex-hull-2d:convex-hull *points*))
```

A list of points belonging to the convex hull can be retrieved with
`convex-hull-2d:convex-hull-points`. They are listed in counterclockwise order.

Here is a total example:

``` lisp
CL-USER> (defparameter *points*
  (loop repeat 30 collect (convex-hull-2d:point (random 1.0) (random 1.0))))
*POINTS*
CL-USER> *points*
((CONVEX-HULL-2D:POINT 0.8369534 0.15845191)
 (CONVEX-HULL-2D:POINT 0.9940388 0.2731737)
 (CONVEX-HULL-2D:POINT 0.7272601 0.28303456)
 (CONVEX-HULL-2D:POINT 0.3029716 0.27735472)
 (CONVEX-HULL-2D:POINT 0.48799133 0.9215044)
 (CONVEX-HULL-2D:POINT 0.109594464 0.5851933)
 (CONVEX-HULL-2D:POINT 0.061249733 0.08545172)
 (CONVEX-HULL-2D:POINT 0.8959471 0.3961028)
 (CONVEX-HULL-2D:POINT 0.07089019 0.638446)
 (CONVEX-HULL-2D:POINT 0.10380125 0.29901338)
 (CONVEX-HULL-2D:POINT 0.314726 0.51293385)
 (CONVEX-HULL-2D:POINT 0.14495802 0.20546556)
 (CONVEX-HULL-2D:POINT 0.203879 0.53706515)
 (CONVEX-HULL-2D:POINT 0.061248064 0.34604192)
 (CONVEX-HULL-2D:POINT 0.77920675 0.037198424)
 (CONVEX-HULL-2D:POINT 0.6712471 0.17420173)
 (CONVEX-HULL-2D:POINT 0.89990485 0.1044451)
 (CONVEX-HULL-2D:POINT 0.33881056 0.40240037)
 (CONVEX-HULL-2D:POINT 0.8393934 0.14111662)
 (CONVEX-HULL-2D:POINT 0.3657521 0.3456192)
 (CONVEX-HULL-2D:POINT 0.30062592 0.68852675)
 (CONVEX-HULL-2D:POINT 0.7178433 0.6040727)
 (CONVEX-HULL-2D:POINT 0.11676514 0.4108305)
 (CONVEX-HULL-2D:POINT 0.11116743 0.22390962)
 (CONVEX-HULL-2D:POINT 0.9239521 0.44130588)
 (CONVEX-HULL-2D:POINT 0.65205526 0.83993924)
 (CONVEX-HULL-2D:POINT 0.13798416 0.13034499)
 (CONVEX-HULL-2D:POINT 0.5514792 0.32335925)
 (CONVEX-HULL-2D:POINT 0.7928457 0.43032002)
 (CONVEX-HULL-2D:POINT 0.8511257 0.3334018))
CL-USER> (defparameter *hull* (convex-hull-2d:convex-hull *points*))
*HULL*
CL-USER> (convex-hull-2d:convex-hull-points *hull*)
((CONVEX-HULL-2D:POINT 0.89990485 0.1044451)
 (CONVEX-HULL-2D:POINT 0.9940388 0.2731737)
 (CONVEX-HULL-2D:POINT 0.9239521 0.44130588)
 (CONVEX-HULL-2D:POINT 0.65205526 0.83993924)
 (CONVEX-HULL-2D:POINT 0.48799133 0.9215044)
 (CONVEX-HULL-2D:POINT 0.07089019 0.638446)
 (CONVEX-HULL-2D:POINT 0.061248064 0.34604192)
 (CONVEX-HULL-2D:POINT 0.061249733 0.08545172)
 (CONVEX-HULL-2D:POINT 0.77920675 0.037198424))
```

Insideness test
---------------

A point can be tested to be inside of the convex hull or not. Firstly, the hull
needs to be triangularized like so:

``` lisp
(defparameter *tris* (convex-hull-2d:triangularize *hull*))
```

Then points can be tested like so:

``` lisp
CL-USER> (convex-hull-2d:inside-convex-hull-p *tris* (convex-hull-2d:point 0.2 0.05))
NIL
CL-USER> (convex-hull-2d:inside-convex-hull-p *tris* (convex-hull-2d:point 0.3 0.1))
T
```
