(in-package #:lift-test)

(deftestsuite test-skipping (lift-test)
  ())

(deftestsuite test-skipping-via-config (test-skipping)
  (config-file)
  (:teardown
   (when (and config-file (probe-file config-file))
     (delete-file config-file))))

;;;;

(deftestsuite test-skipping-helper ()
  ())

(addtest (test-skipping-helper)
  test-1
  (ensure t))

(deftestsuite test-skipping-helper-a (test-skipping-helper)
  ())

(addtest (test-skipping-helper-a)
  test-1
  (ensure t))

(addtest (test-skipping-helper-a)
  test-2
  (ensure t))

(addtest (test-skipping-helper-a)
  test-3
  (ensure t))

(deftestsuite test-skipping-helper-a-1 (test-skipping-helper-a)
  ())

(addtest (test-skipping-helper-a-1)
  test-1
  (ensure t))

(addtest (test-skipping-helper-a-1)
  test-2
  (ensure t))

(deftestsuite test-skipping-helper-a-2 (test-skipping-helper-a)
  ())

(addtest (test-skipping-helper-a-2)
  test-1
  (ensure t))

(addtest (test-skipping-helper-a-2)
  test-2
  (ensure t))


;;;;

(addtest (test-skipping)
  nothing-skipped
  (run-tests :suite 'test-skipping-helper-a)
  (ensure-same (length (tests-run *test-result*)) 7 :test '=)
  (ensure-null (skipped-test-cases *test-result*)))

(addtest (test-skipping)
  skip-testsuite
  (run-tests :suite 'test-skipping-helper-a
	     :skip-tests '(test-skipping-helper-a-2))
  (ensure-same (length (tests-run *test-result*)) 5 :test '=)
  (ensure-same (length (skipped-test-cases *test-result*)) 0 :test '=))

(addtest (test-skipping)
  skip-test-case
  (run-tests :suite 'test-skipping-helper-a
	     :skip-tests '((test-skipping-helper-a-2 test-1)))
  (ensure-same (length (tests-run *test-result*)) 6 :test '=)
  (ensure-same (length (skipped-test-cases *test-result*)) 1 :test '=))

(addtest (test-skipping)
  skip-mix
  (run-tests :suite 'test-skipping-helper-a
	     :skip-tests '((test-skipping-helper-a-2 test-1)
			   test-skipping-helper-a-1
			   (test-skipping-helper-a test-2)))
  (ensure-same (length (tests-run *test-result*)) 3 :test '=)
  (ensure-same (length (skipped-test-cases *test-result*)) 2 :test '=))

(addtest (test-skipping)
  skip-subclasses
  (let ((r (run-tests :suite 'test-skipping-helper
		      :skip-tests '(test-skipping-helper-a))))
    (ensure-same (length (tests-run r)) 1 :test '=)
    (ensure-same (length (skipped-test-cases r)) 0 :test '=)
    (ensure-same (length (skipped-testsuites r)) 3 :test '=)))

;;;;

(defun write-config (lines)
  (let ((filename (lift::unique-filename "tempoary.config")))
    (with-open-file (out filename 
			 :if-exists :error
			 :if-does-not-exist :create
			 :direction :output)
      (write-string lines out)
      (format out "~%~%\(test-skipping-helper-a\)~%"))
    filename))

(addtest (test-skipping-via-config)
  no-skips
  (setf config-file (write-config ""))
  (run-tests :config config-file)
  (ensure-same (length (tests-run *test-result*)) 7 :test '=)
  (ensure-null (skipped-test-cases *test-result*)))

(addtest (test-skipping-via-config)
  skip-testsuite-1
  (setf config-file (write-config "(:skip-tests test-skipping-helper-a-2)"))
  (run-tests :config config-file)
  (ensure-same (length (tests-run *test-result*)) 5 :test '=)
  (ensure-same (length (skipped-test-cases *test-result*)) 0 :test '=))

(addtest (test-skipping-via-config)
  skip-testsuite-2
  (setf config-file 
	(write-config "(:skip-testsuites test-skipping-helper-a-2)"))
  (run-tests :config config-file)
  (ensure-same (length (tests-run *test-result*)) 5 :test '=)
  (ensure-same (length (skipped-test-cases *test-result*)) 0 :test '=))

(addtest (test-skipping-via-config)
  skip-testsuite-3
  (setf config-file 
	(write-config "(:skip-tests (test-skipping-helper-a-2))"))
  (run-tests :config config-file)
  (ensure-same (length (tests-run *test-result*)) 5 :test '=)
  (ensure-same (length (skipped-test-cases *test-result*)) 0 :test '=))

(addtest (test-skipping-via-config)
  skip-test-case
  (setf config-file 
	(write-config "(:skip-tests (test-skipping-helper-a-2 test-1))"))
  (run-tests :config config-file)
  (ensure-same (length (tests-run *test-result*)) 6 :test '=)
  (ensure-same (length (skipped-test-cases *test-result*)) 1 :test '=))

(addtest (test-skipping-via-config)
  skip-mix-1
  (setf config-file
	(write-config "(:skip-tests (test-skipping-helper-a-2 test-1)
			   test-skipping-helper-a-1
			   (test-skipping-helper-a test-2))"))
  (run-tests :config config-file)
  (ensure-same (length (tests-run *test-result*)) 3 :test '=)
  (ensure-same (length (skipped-test-cases *test-result*)) 2 :test '=))

(addtest (test-skipping-via-config)
  skip-mix-2
  (setf config-file
	(write-config "
(:skip-tests (test-skipping-helper-a-2 test-1))
(:skip-testsuites test-skipping-helper-a-1)
(:skip-tests (test-skipping-helper-a test-2))"))
  (run-tests :config config-file)
  (ensure-same (length (tests-run *test-result*)) 3 :test '=)
  (ensure-same (length (skipped-test-cases *test-result*)) 2 :test '=))
