(in-package #:common-lisp-user)

(defpackage #:lift-test 
  (:use #:common-lisp #:lift)
  (:import-from #:lift
                #:failures
                #:errors
                #:tests-run
		#:skipped-test-cases
		#:skipped-testsuites
                #:test-mode
                #:test-interactive?
                #:make-test-result
                #:testsuite-test-count
		#:*test-maximum-error-count*
		#:*test-maximum-failure-count*
		#:failures
		#:errors))
