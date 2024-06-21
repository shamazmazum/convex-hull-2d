(defun do-all()
  (ql:quickload :convex-hull-2d/tests)
  (uiop:quit
   (if (uiop:call-function "convex-hull-2d/tests:run-tests")
       0 1)))

(do-all)
