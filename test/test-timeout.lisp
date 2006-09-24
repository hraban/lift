(in-package #:lift-test)

(deftestsuite test-lift-timeouts (lift-test)
  ())

(deftestsuite test-timeout-helper () 
  ()
  :timeout 1)

(addtest (test-timeout-helper)
  success-1
  (ensure t))

(addtest (test-timeout-helper)
  timeout-1
  (sleep 2)
  (ensure t))


