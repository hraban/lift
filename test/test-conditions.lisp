(in-package #:lift-test)

(deftestsuite test-normal-conditions (lift-test)
  ())

(deftestsuite test-normal-conditions-helper (lift-test)
  ())

(define-condition test-simple-condition-condition-1 () ())

(define-condition test-simple-condition-condition-2 () ())

(addtest (test-normal-conditions-helper)
  signal-simple-condition-1
  (ensure-condition test-simple-condition-condition-2
    (signal 'test-simple-condition-condition-1)
    (signal 'test-simple-condition-condition-2)))

(addtest (test-normal-conditions-helper)
  signal-simple-condition-2
  (ensure-condition test-simple-condition-condition-2
    (signal 'test-simple-condition-condition-2)
    (signal 'test-simple-condition-condition-1)))

(deftestsuite test-normal-conditions-helper-2 (lift-test)
  ())

(addtest (test-normal-conditions-helper-2)
  signal-simple-condition-1
  (ensure-condition (test-simple-condition-condition-1
		     :catch-all-conditions? t)
    (signal 'test-simple-condition-condition-1)
    (signal 'test-simple-condition-condition-2)))

(addtest (test-normal-conditions)
  test-1
  (let ((r (run-tests :suite 'test-normal-conditions-helper)))
    (ensure-same (length (tests-run r)) 2 :test '=)
    (ensure-same (length (failures r)) 0)
    (ensure-same (length (errors r)) 0)))

(addtest (test-normal-conditions)
  test-2
  (let ((r (run-tests :suite 'test-normal-conditions-helper-2)))
    (ensure-same (length (tests-run r)) 1 :test '=)
    (ensure-same (length (failures r)) 0)
    (ensure-same (length (errors r)) 0)))
