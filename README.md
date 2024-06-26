convex-hull-2d
=============
This library is for calculation of the convex hull for a set of two-dimensional
points.

Convex hulls
------------

Let's create a set of random points:

``` lisp
(defparameter *points*
  (loop repeat 30 collect (convex-hull-2d:point (random 1d0) (random 1d0))))
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
  (loop repeat 30 collect (convex-hull-2d:point (random 1d0) (random 1d0))))
*POINTS*
CL-USER> *points*
((CONVEX-HULL-2D:POINT 0.6836942046284917d0 0.5566406444542114d0)
 (CONVEX-HULL-2D:POINT 0.6333417494758864d0 0.16322128001846692d0)
 (CONVEX-HULL-2D:POINT 0.910786927070206d0 0.12547213520723144d0)
 (CONVEX-HULL-2D:POINT 0.32087315939652483d0 0.42367670400696444d0)
 (CONVEX-HULL-2D:POINT 0.6396886506191448d0 0.46244251931231206d0)
 (CONVEX-HULL-2D:POINT 0.8528544539291245d0 0.153977097851405d0)
 (CONVEX-HULL-2D:POINT 0.4505245695774711d0 0.337405667279878d0)
 (CONVEX-HULL-2D:POINT 0.08422326000156688d0 0.925235155248262d0)
 (CONVEX-HULL-2D:POINT 0.5059618746293351d0 0.02929888149260096d0)
 (CONVEX-HULL-2D:POINT 0.45360980161988196d0 0.7139487949345966d0)
 (CONVEX-HULL-2D:POINT 0.48955122072250457d0 0.9705286074692296d0)
 (CONVEX-HULL-2D:POINT 0.21179897009426019d0 0.6276055290238567d0)
 (CONVEX-HULL-2D:POINT 0.48183156402182914d0 0.9804978494899446d0)
 (CONVEX-HULL-2D:POINT 0.0057759874962375335d0 0.4150633193619584d0)
 (CONVEX-HULL-2D:POINT 0.26647897879295823d0 0.7417656680447378d0)
 (CONVEX-HULL-2D:POINT 0.6706368096790769d0 0.9366937073661066d0)
 (CONVEX-HULL-2D:POINT 0.29326506959894694d0 0.32341966361754393d0)
 (CONVEX-HULL-2D:POINT 0.8030136621779038d0 0.3011412464694454d0)
 (CONVEX-HULL-2D:POINT 0.9078991057601653d0 0.06726746764449398d0)
 (CONVEX-HULL-2D:POINT 0.473334828570545d0 0.9502496746784499d0)
 (CONVEX-HULL-2D:POINT 0.9369855174023323d0 0.6868031660247396d0)
 (CONVEX-HULL-2D:POINT 0.8729989759389014d0 0.46893808620257293d0)
 (CONVEX-HULL-2D:POINT 0.43166600718903503d0 0.9684783626219589d0)
 (CONVEX-HULL-2D:POINT 0.017339761939510367d0 0.11305157797098397d0)
 (CONVEX-HULL-2D:POINT 0.32821909457167275d0 0.004379790956820173d0)
 (CONVEX-HULL-2D:POINT 0.696888509182571d0 0.28698227156577105d0)
 (CONVEX-HULL-2D:POINT 0.3185855656505543d0 0.27038938272057167d0)
 (CONVEX-HULL-2D:POINT 0.5413978720285979d0 0.6863100877141444d0)
 (CONVEX-HULL-2D:POINT 0.23167158569924662d0 0.4872508694902682d0)
 (CONVEX-HULL-2D:POINT 0.6630292518372782d0 0.8300692678857073d0))
CL-USER> (defparameter *hull* (convex-hull-2d:convex-hull *points*))
*HULL*
CL-USER> (convex-hull-2d:convex-hull-points *hull*)
((CONVEX-HULL-2D:POINT 0.9078991057601653d0 0.06726746764449398d0)
 (CONVEX-HULL-2D:POINT 0.910786927070206d0 0.12547213520723144d0)
 (CONVEX-HULL-2D:POINT 0.9369855174023323d0 0.6868031660247396d0)
 (CONVEX-HULL-2D:POINT 0.6706368096790769d0 0.9366937073661066d0)
 (CONVEX-HULL-2D:POINT 0.48183156402182914d0 0.9804978494899446d0)
 (CONVEX-HULL-2D:POINT 0.08422326000156688d0 0.925235155248262d0)
 (CONVEX-HULL-2D:POINT 0.0057759874962375335d0 0.4150633193619584d0)
 (CONVEX-HULL-2D:POINT 0.017339761939510367d0 0.11305157797098397d0)
 (CONVEX-HULL-2D:POINT 0.32821909457167275d0 0.004379790956820173d0))
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
CL-USER> (convex-hull-2d:inside-convex-hull-p *tris* (convex-hull-2d:point 2d-1 5d-2))
T
CL-USER> (convex-hull-2d:inside-convex-hull-p *tris* (convex-hull-2d:point 9d-1 9d-1))
NIL
```

Perimeter and surface
--------------------

To obtain perimeter and surface of the convex hull use these functions:

``` lisp
CL-USER> (convex-hull-2d:convex-hull-perimeter *tris*)
3.3114985547459437d0
CL-USER> (convex-hull-2d:convex-hull-surface *tris*)
0.7756433042405304d0
```
