(in-package #:lift)

(defvar *in-middle-of-failure?* t)

(defun run-test (&key
		 (name *last-test-case-name*)
		 (suite *last-testsuite-name*) 
		 (break-on-errors? *test-break-on-errors?*)
		 (break-on-failures? *test-break-on-failures?*)
		 (result nil)
		 (profile *profile-style* profile-supplied?)
                 (testsuite-initargs nil))
  "Run a single test-case in a testsuite. Will run the most recently
defined or run testcase unless the name and suite arguments are used
to override them."
  (assert suite nil "Test suite could not be determined.")
  (assert name nil "Test-case could not be determined.")
  (when profile-supplied?
    (push profile testsuite-initargs)
    (push :profile testsuite-initargs))
  (let* ((*test-break-on-errors?* break-on-errors?)
	 (*test-break-on-failures?* break-on-failures?))
    (unless result
      (setf result (make-test-result 
		    suite :single :testsuite-initargs testsuite-initargs)))
    (prog1
	(let ((*current-test-case-name* (find-test-case suite name :errorp t))
	      (*test-result* result))
	  (do-testing-in-environment
	      suite result 
	      (lambda () 
		(run-test-internal *current-test* *current-test-case-name* result))))
      (setf *test-result* result)
      (setf *last-test-case-name* (find-test-case suite name)
	    *last-testsuite-name* suite))))

(defun do-testing-in-environment (suite-name result fn)
  (let ((suite nil)
	(*current-testsuite-name* suite-name))
    (catch :test-end
      (tagbody 
       :test-start
	 (restart-case
	     (handler-bind ((warning #'muffle-warning)       
					; ignore warnings... 
			    #+(and allegro)
			    (excl:interrupt-signal 
			     (lambda (_)
			       (declare (ignore _))
			       (cancel-testing :interrupt)))
			    (error 
			     (lambda (condition)
			       (handle-error-while-testing
				condition 'testsuite-error suite-name result)
			       (go :test-end)))
			    (serious-condition 
			     (lambda (condition)
			       (handle-error-while-testing
				condition 'testsuite-serious-condition
				suite-name result)
			       (go :test-end))))
	       (setf (current-step result) :create)
	       (setf suite (make-testsuite 
			    suite-name (testsuite-initargs result)))
	       (let ((*current-test* suite))
		 (unwind-protect
		      (let ((*lift-equality-test* (equality-test suite)))
			(%start-test-suite (type-of suite) result)
			(testsuite-setup suite result)
			(do-testing suite result fn)
			result)
		   ;; cleanup
		   (testsuite-teardown suite result))))
	   (ensure-failed (condition) 
	     :test (lambda (c) (declare (ignore c)) *in-middle-of-failure?*)
	     (report-test-problem
	      'testsuite-failure result suite-name 
	      *current-test-case-name* condition))
	   (retry-test () 
	     :report (lambda (s) (format s "Re-run testsuite ~a"
					 *current-testsuite-name*))
	     (go :test-start)))
       :test-end)))
  (values result))

(defmethod do-testing ((suite test-mixin) result fn)
  (funcall fn)
  result)

(defun run-tests (&rest args &key 
		  (suite nil)
		  (break-on-errors? *test-break-on-errors?*)
		  (break-on-failures? *test-break-on-failures?*)
		  (config nil)
		  (dribble *lift-dribble-pathname*)
		  (report-pathname *lift-report-pathname*)
		  (profile *profile-style* profile-supplied?)
		  (skip-tests *skip-tests*)
		  ;(timeout nil)
		  (do-children? *test-run-subsuites?*)
		  (testsuite-initargs nil) 
		  result
		  &allow-other-keys)
  "Run all of the tests in a suite." 
  (prog1
      (let ((args-copy (copy-list args)))
	(remf args :suite)
	(remf args :break-on-errors?)
	(remf args :break-on-failures?)
	(remf args :run-setup)
	(remf args :dribble)
	(remf args :config)
	(remf args :skip-tests)
	(remf args :report-pathname)
	(remf args :do-children?)
	(remf args :testsuite-initargs)
	(remf args :profile)
	(when profile-supplied?
	  (push profile testsuite-initargs)
	  (push :profile testsuite-initargs))
	(let* ((*lift-report-pathname*
		(cond ((null report-pathname) nil)
		      ((eq report-pathname t)
		       (report-summary-pathname))
		      (t
		       report-pathname)))
	       (*test-run-subsuites?* do-children?)
	       (*skip-tests* (canonize-skip-tests skip-tests))
	       (*print-readably* nil)
	       (report-pathname *lift-report-pathname*))
	  (when report-pathname
	    (ensure-directories-exist report-pathname))
	  (cond ((and suite config)
		 (error "Specify either configuration file or test suite 
but not both."))
		(config
		 (unless result
		   (setf result
			 (apply #'make-test-result config :multiple 
				:testsuite-initargs testsuite-initargs 
				args)))
		 (when report-pathname
		   (write-log-header report-pathname result args-copy))
		 (let* ((*test-result* result))
		   (setf result (run-tests-from-file config))))
		((or suite (setf suite *last-testsuite-name*))
		 (unless result
		   (setf result
			 (apply #'make-test-result suite 
				:multiple :testsuite-initargs testsuite-initargs
				args)))
		 (setf (testsuite-initargs result) testsuite-initargs)
		 (when report-pathname
		   (write-log-header report-pathname result args-copy))
		 (let* ((*test-break-on-errors?* break-on-errors?)
			(*test-break-on-failures?* break-on-failures?)
			(*test-result* result)
			(dribble-stream
			 (when dribble
			   (open dribble
				 :direction :output
				 :if-does-not-exist :create
				 :if-exists *lift-if-dribble-exists*)))
			(*lift-standard-output* 
			 (maybe-add-dribble 
			  *lift-standard-output* dribble-stream))
			(*standard-output* *lift-standard-output*)
			(*error-output* (maybe-add-dribble 
					 *error-output* dribble-stream))
			(*debug-io* (maybe-add-dribble 
				     *debug-io* dribble-stream))
			(*lift-debug-output* (maybe-add-dribble 
					      *lift-debug-output* dribble-stream)))
		   (unwind-protect
			(restart-case
			    (run-tests-internal suite result)
			   (cancel-testing (&optional (result *test-result*))
			     :report (lambda (stream) 
				       (format stream "Cancel testing of ~a"
					       *current-testsuite-name*))
			     (declare (ignore result))
			     (values nil t)))
		     ;; cleanup
		     (when dribble-stream 
		       (close dribble-stream)))
		   ;; FIXME -- ugh!
		   (setf (tests-run result) (reverse (tests-run result)))
		   (when report-pathname
		     (write-log-footer report-pathname result))
		   (values result)))
		(t
		 (error "There is not a current test suite and neither suite 
nor configuration file options were specified.")))))
	(setf *test-result* result)))

(defun run-tests-internal (suite-name result)
  (dolist (suite-name (if *test-run-subsuites?* 
			  (collect-testsuites suite-name)
			  (list suite-name)))
    (do-testing-in-environment
	suite-name result
	(lambda ()
	  (testsuite-run *current-test* result)))
    (setf *test-result* result)))

(defun testsuite-run (testsuite result)
  "Run the cases in `testsuite`"
  (let* ((methods (testsuite-methods testsuite))
	 (suite-name (class-name (class-of testsuite)))
	 (*current-testsuite-name* suite-name)
	 data)
    (cond ((skip-test-suite-children-p result suite-name)
	   (skip-testsuite result suite-name))
	  (t
	   (unless (start-time result)
	     (setf (start-time result) (get-internal-real-time)
		   (start-time-universal result) (get-universal-time)))
	   (unwind-protect
		(loop for method in methods do
		     (unwind-protect
			  (progn
			    (write-log-test-start :save suite-name method
						  :stream *lift-report-pathname*)
			    (setf data 
				  (if (skip-test-case-p result suite-name method)
				      `(:problem ,(skip-test-case
						   result suite-name method))
				      (run-test-internal testsuite method result))))
		       (when *lift-report-pathname*
			 (write-log-test-end
			  :save suite-name method data
			  :stream *lift-report-pathname*))))
	     (setf (end-time result) (get-universal-time)))))
    (setf *last-testsuite-name* suite-name)))

(defmethod do-test ((suite test-mixin) name result)
  (declare (ignore result))
  (let* ((suite-name (class-name (class-of suite)))
	 (fn (gethash name (test-name->methods suite-name))))
    (if fn
	(funcall fn suite)
	(error "expected to find ~a test for ~a but didn't" name suite-name))))

(defun run-test-internal (suite test-case-name result)
  (let* ((result-pushed? nil)
	 (suite-name (class-name (class-of suite)))
	 (*current-test-case-name* test-case-name)
	 (*current-testsuite-name* suite-name)
	 (error nil)
	 (current-condition nil))
    (loop for case in (ensure-list
		       (test-case-option suite-name test-case-name :depends-on))
       unless (test-case-tested-p suite-name case) do
	 (run-test-internal suite case result))
    (flet ((maybe-push-result ()
	     (let ((datum (list suite-name test-case-name (test-data suite))))
	       (cond ((null result-pushed?)
		      (setf result-pushed? t)
		      (push datum (tests-run result)))
		     (t
		      ;; replace
		      (setf (first (tests-run result)) datum))))))
      (%start-test-case test-case-name result)
      (tagbody 
       :test-start
	 (restart-case
	     (handler-bind ((warning #'muffle-warning)       
					; ignore warnings... 
			    #+(and allegro)
			    (excl:interrupt-signal 
			     (lambda (_)
			       (declare (ignore _))
			       (cancel-testing :interrupt)))
			    (error 
			     (lambda (condition)
			       (handle-error-while-testing
				condition 'test-error suite-name result)
			       (go :test-end)))
			    (serious-condition 
			     (lambda (condition)
			       (handle-error-while-testing
				condition 'test-serious-condition
				suite-name result)
			       (go :test-end))))
	       (setf (current-method suite) test-case-name)
	       (set-test-case-options suite-name test-case-name)
	       (record-start-times result suite)
	       (unwind-protect
		    (progn
		      (setup-test suite)
		      (setf (current-step result) :testing)
		      (multiple-value-bind (result measures error-condition)
			  (while-measuring (t measure-space measure-seconds)
			    (do-test suite test-case-name result))
			(declare (ignore result))
			(setf error error-condition)
			(destructuring-bind (space seconds) measures
			  (setf (getf (test-data suite) :seconds) seconds
				(getf (test-data suite) :conses) space)))
		      (when error
			(error error))
		      (check-for-surprises suite-name test-case-name))
		 ;; cleanup
		 (maybe-push-result)
		 (when (run-teardown-p suite :test-case)
		   (test-case-teardown suite result))
		 (record-end-times result suite)))
	   (ensure-failed (condition) 
	     :test (lambda (c) (declare (ignore c)) 
			   *in-middle-of-failure?*)
	     (report-test-problem
	      'test-failure result suite-name
	      *current-test-case-name* condition)
	     (setf current-condition condition)
	     (go :test-failed))
	   (test-failed (condition) 
	     :test (lambda (c) (declare (ignore c)) 
			   *in-middle-of-failure?*)
	     (setf current-condition condition)
	     (go :test-failed))
	   (retry-test () 
	     :report (lambda (s) (format s "Re-run test-case ~a"
			     *current-test-case-name*))
	     (go :test-start)))
	 :test-failed
	 (if (and *test-break-on-failures?*
		  (not (test-case-expects-failure-p 
			suite-name test-case-name)))
	     (let ((*in-middle-of-failure?* nil))
	       (invoke-debugger current-condition))
	     (go :test-end))
       :test-end)
      (maybe-push-result)))
  (when *test-print-test-case-names*
    (when (not (eq *test-print-test-case-names* :brief))
      (format *lift-debug-output* "~40T"))
    (print-lift-message "~a"
                        (result-summary-tag (getf (test-data suite) :problem)
                                            *test-print-test-case-names*)))
  (setf *current-test-case-name* test-case-name	*test-result* result)
  (third (first (tests-run result))))

(defun handle-error-while-testing (condition error-class suite-name result)
  (let ((*in-middle-of-failure?* nil))
    (report-test-problem
     error-class result suite-name
     *current-test-case-name* condition
     :backtrace (get-backtrace condition))
    (when (and *test-break-on-errors?*
	       (not (test-case-expects-error-p 
		     *current-testsuite-name* *current-test-case-name*)))
      (invoke-debugger condition))))

(defun maybe-add-dribble (stream dribble-stream)
  (if dribble-stream
      (values (make-broadcast-stream stream dribble-stream) t)
      (values stream nil)))

