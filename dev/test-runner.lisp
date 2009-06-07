(in-package #:lift)

(defvar *in-middle-of-failure?* t)

(defun run-test (&rest args
		 &key (test-case *current-test-case-name*)
		 (name test-case name-supplied-p)
		 (suite *current-testsuite-name*) 
		 (break-on-errors? *test-break-on-errors?*)
		 (break-on-failures? *test-break-on-failures?*)
		 (do-children? *test-run-subsuites?*)
		 (result nil)
		 (profile nil)
                 (testsuite-initargs nil))
  "Run a single testcase in a test suite. Will run the most recently defined or run testcase unless the name and suite arguments are used to override them."
  (when name-supplied-p
    (setf test-case name))
  (assert suite nil "Test suite could not be determined.")
  (assert test-case nil "Test-case could not be determined.")
  (let ((args-copy (copy-list args)))
    (declare (ignore args-copy))
    (remf args :suite)
    (remf args :break-on-errors?)
    (remf args :break-on-failures?)
    (remf args :run-setup)
    (remf args :dribble)
    (remf args :config)
    (remf args :report-pathname)
    (remf args :do-children?)
    (remf args :skip-tests)
    (remf args :testsuite-initargs)
    (let* ((*test-break-on-errors?* break-on-errors?)
	   (*test-break-on-failures?* break-on-failures?)
	   (*test-run-subsuites?* do-children?)
	   (*current-test*
	    (make-testsuite 
	     suite 
	     (if (find :profile testsuite-initargs)
		 testsuite-initargs
		 (setf testsuite-initargs
		       `(:profile ,profile ,@testsuite-initargs))))))
      (unless result
	(setf result (make-test-result suite :single)))
      (prog1
	  (let ((*current-test-case-name* (find-test-case suite test-case))
		(*current-testsuite-name* suite)
		(*test-result* result))
	    (do-testing-in-environment
		*current-test* result 
		(lambda () 
		  (apply #'run-test-internal
		   *current-test* *current-test-case-name* result nil))))
	(setf *test-result* result)
	(setf *current-test-case-name* (find-test-case suite test-case)
	      *current-testsuite-name* suite)))))

(defmethod do-testing-in-environment :around ((suite test-mixin) result fn)
  (declare (ignore fn))
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
			      condition 'testsuite-error suite result)
			     (go :test-end)))
			  (serious-condition 
			   (lambda (condition)
			     (handle-error-while-testing
			      condition 'testsuite-serious-condition
			      suite result)
			     (go :test-end))))
	     (unwind-protect
		  (let ((*lift-equality-test* (equality-test suite)))
		    (testsuite-setup suite result)
		    (call-next-method)
		    result)
	       ;; cleanup
	       (testsuite-teardown suite result)))
	 (ensure-failed (condition) 
	   :test (lambda (c) (declare (ignore c)) *in-middle-of-failure?*)
	   (report-test-problem
	    'testsuite-failure result suite 
	    *current-test-case-name* condition))
	 (retry-test () :report "Retry the test." 
		     (go :test-start)))
     :test-end))
  (values result))

(defmethod do-testing-in-environment ((suite test-mixin) result fn)
  (do-testing suite result fn)
  (values result))

(defmethod do-testing ((suite test-mixin) result fn)
  (funcall fn)
  (values result))

(defmethod run-tests-internal ((suite symbol) &rest args
			       &key &allow-other-keys)
  (let ((*current-test* (make-testsuite suite args))
	(passthrough-arguments nil))
    (loop for arg in '(:result :do-children?) 
       when (getf args arg) do
	 (push (getf args arg) passthrough-arguments)
	 (push arg passthrough-arguments))
    (apply #'run-tests-internal *current-test* passthrough-arguments)))

(defmethod run-tests-internal 
    ((case test-mixin) &key 
     (result (make-test-result (class-of case) :multiple))
     (do-children? *test-run-subsuites?*))
  (let ((*test-run-subsuites?* do-children?))
    (do-testing-in-environment
	case result
	(lambda ()
	  (testsuite-run case result)))
    (setf *test-result* result)))

(defun run-tests (&rest args &key 
		  (suite nil)
		  (break-on-errors? *test-break-on-errors?*)
		  (break-on-failures? *test-break-on-failures?*)
		  (config nil)
		  (dribble *lift-dribble-pathname*)
		  (report-pathname t)
		  (profile nil)
		  (skip-tests *skip-tests*)
		  ;(timeout nil)
		  (do-children? *test-run-subsuites?*)
		  (testsuite-initargs nil) 
		  result
		  &allow-other-keys)
  "Run all of the tests in a suite." 
  (declare (ignore profile))
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
	(let* ((*lift-report-pathname*
		(cond ((null report-pathname) nil)
		      ((eq report-pathname t)
		       (report-summary-pathname))))
	       (*test-run-subsuites?* do-children?)
	       (*skip-tests* skip-tests)
	       (*print-readably* nil)
	       (report-pathname *lift-report-pathname*))
	  (canonize-skip-tests)
	  (when report-pathname
	    (ensure-directories-exist report-pathname))
	  (cond ((and suite config)
		 (error "Specify either configuration file or test suite 
but not both."))
		(config
		 (unless result
		   (setf result
			 (apply #'make-test-result config :multiple args)))
		 (when report-pathname
		   (write-report-header report-pathname result args-copy))
		 (let* ((*test-result* result))
		   (setf result (run-tests-from-file config))))
		((or suite (setf suite *current-testsuite-name*))
		 (unless result
		   (setf result
			 (apply #'make-test-result suite :multiple args)))
		 (when report-pathname
		   (write-report-header report-pathname result args-copy))
		 (let* ((*test-break-on-errors?* break-on-errors?)
			(*test-break-on-failures?* break-on-failures?)
			(*test-result* result)
			(dribble-stream
			 (when dribble
			   (open dribble
				 :direction :output
				 :if-does-not-exist :create
				 :if-exists *lift-if-dribble-exists*)))
			(*standard-output* 
			 (maybe-add-dribble 
			  *lift-standard-output* dribble-stream))
			(*error-output* (maybe-add-dribble 
					 *error-output* dribble-stream))
			(*debug-io* (maybe-add-dribble 
				     *debug-io* dribble-stream)))
		   (unwind-protect
			(restart-case
			    (dolist (testsuite (if (consp suite) 
						   suite (list suite)))
			      (let ((*current-testsuite-name* testsuite))
				(apply #'run-tests-internal testsuite
				       :result result
				       testsuite-initargs))
			      (setf *current-testsuite-name* testsuite))
			   (cancel-testing (result)
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
		     (write-report-footer report-pathname result))
		   (values result)))
		(t
		 (error "There is not a current test suite and neither suite 
nor configuration file options were specified.")))))
	(setf *test-result* result)))

(defmethod testsuite-run ((testsuite test-mixin) (result test-result))
  (unless (start-time result)
    (setf (start-time result) (get-internal-real-time)
	  (start-time-universal result) (get-universal-time)))
  (unwind-protect
       (let* ((methods (testsuite-methods testsuite))
	      (suite-name (class-name (class-of testsuite)))
	      (*current-testsuite-name* suite-name))
	 (loop for method in methods do
	      (if (skip-test-case-p result suite-name method)
		  (skip-test-case result suite-name method)
		  (run-test-internal testsuite method result)))
	 (when *test-run-subsuites?*
	   (if (skip-test-suite-children-p result suite-name)
	       (skip-testsuite result suite-name)
	       (loop for subclass in (direct-subclasses (class-of testsuite))	
		  when (and (testsuite-p subclass)
			    (not (member (class-name subclass) 
					 (suites-run result)))) do
		  (run-tests-internal (class-name subclass)
				      :result result)))))
    (setf (end-time result) (get-universal-time))))

(defmethod run-test-internal ((suite symbol) (name symbol) result
			       &rest args &key &allow-other-keys)
  (let ((*current-test* (make-testsuite suite args))
	(passthrough-arguments nil))
    (loop for arg in '(:result :do-children?) 
       when (getf args arg) do
	 (push (getf args arg) passthrough-arguments)
	 (push arg passthrough-arguments))
    (apply #'run-test-internal 
	   *current-test* name result passthrough-arguments)))

(defmethod run-test-internal ((suite test-mixin) (name symbol) result
			      &rest _)
  (declare (ignore _))
  (let ((result-pushed? nil)
	(*current-test-case-name* name)
	(suite-name (class-name (class-of suite)))
	(error nil))
    (flet ((maybe-push-result ()
	     (let ((datum (list (type-of suite)
				*current-test-case-name* (test-data suite))))
	       (cond ((null result-pushed?)
		      (setf result-pushed? t)
		      (push datum (tests-run result)))
		     (t
		      ;; replace
		      (setf (first (tests-run result)) datum))))))
      (when (and *test-print-test-case-names*
		 (eq (test-mode result) :multiple))
	(print-lift-message "~&  run: ~a" name))
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
				condition 'test-error suite result)
			       (go :test-end)))
			    (serious-condition 
			     (lambda (condition)
			       (handle-error-while-testing
				condition 'test-serious-condition suite result)
			       (go :test-end))))
	       (setf (current-method suite) name)
	       (record-start-times suite)
	       (unwind-protect
		    (progn
		      (setup-test suite)
		      (setf (current-step suite) :testing)
		      (multiple-value-bind (result measures error-condition)
			  (while-measuring (t measure-space measure-seconds)
			    (let ((fn (gethash name (test-name->methods suite-name))))
			      (if fn
				(funcall fn suite)
				(error "expected to find ~a test for ~a but didn't" name suite-name))
			      #+(or)
			      (lift-test suite name)))
			(declare (ignore result))
			(setf error error-condition)
			(destructuring-bind (space seconds) measures
			  (setf (getf (test-data suite) :seconds) seconds
				(getf (test-data suite) :conses) space)))
		      (when error
			(error error))
		      (check-for-surprises suite))
		 ;; cleanup
		 (maybe-push-result)
		 (test-case-teardown suite result)
		 (record-end-times result suite)))
	   (ensure-failed (condition) 
	     :test (lambda (c) (declare (ignore c)) *in-middle-of-failure?*)
	     (report-test-problem
	      'test-failure result suite 
	      *current-test-case-name* condition)
	     (if (and *test-break-on-failures?*
		      (not (testcase-expects-failure-p)))
		 (let ((*in-middle-of-failure?* nil))
		   (invoke-debugger condition))
		 (go :test-end)))
	   (retry-test () :report "Retry the test." 
		       (go :test-start)))
       :test-end)
      (maybe-push-result))
    (when *lift-report-pathname*
      (let ((current (first (tests-run result))))
	(summarize-single-test  
	 :save (first current) (second current) (third current)
	 :stream *lift-report-pathname*))))
  (setf *current-test-case-name* name
	*test-result* result))


(defun handle-error-while-testing (condition error-class suite result)
  (report-test-problem
   error-class result suite
   *current-test-case-name* condition
   :backtrace (get-backtrace condition))
  (when (and *test-break-on-errors?*
	     (not (testcase-expects-error-p)))
    (invoke-debugger condition)))

(defun maybe-add-dribble (stream dribble-stream)
  (if dribble-stream
      (values (make-broadcast-stream stream dribble-stream) t)
      (values stream nil)))

