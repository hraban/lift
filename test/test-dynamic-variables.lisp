(in-package #:lift-test)

(defvar *a* 1)

(deftestsuite test-dynamic-variables-1 (lift-test)
  ()
  (:dynamic-variables (*a* 2))
  (:test (test-1 (ensure-same *a* 2))))

(deftestsuite test-dynamic-variables-2 ()
  ()
  (:test (test-1 (ensure-same *a* 1))))

