(defsystem :convex-hull-2d
  :name :convex-hull-2d
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :description "Calculation of the convex hull for a set of two-dimensional points"
  :licence "2-clause BSD"
  :pathname "src"
  :components ((:file "package")
               (:file "convex-hull" :depends-on ("package")))
  :in-order-to ((test-op (load-op "convex-hull-2d/tests")))
  :perform (test-op (op system)
                    (declare (ignore op system))
                    (funcall
                     (symbol-function
                      (intern (symbol-name '#:run-tests)
                              (find-package :convex-hull-2d/tests)))))
  :depends-on (:serapeum))

(defsystem :convex-hull-2d/tests
  :name :convex-hull-2d/tests
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :licence "2-clause BSD"
  :pathname "tests"
  :components ((:file "package")
               (:file "tests" :depends-on ("package")))
  :depends-on (:convex-hull-2d :fiveam))
