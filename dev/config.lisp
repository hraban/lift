(in-package #:lift)

#|
(:report-property :if-exists :supersede)
(:report-property :unique-name nil)
(:report-property :format :html)
(:report-property :name "index")
(:report-property :relative-to db.agraph.tests)

For text based reports like :describe, the report name is the filename
where the report is placed or a stream (e.g., *standard-output*).

The :name property specifies the name and type.

There are three ways to specify the directory:

1. :full-name
2. :relative-to
3. the current directory (via *default-pathname-defaults*)

If :full-name is a pathname with a name and type, then these will be
used rather than :name. If :unique-name is true (and the destination
is not a stream), then the date and an integer tag will be added to the 
name. E.g., the path `/tmp/lift-tests/report.txt` will become 
`/tmp/lift-tests/report-2009-02-01-1.txt`.


For HTML, The report name specifies a _directory_. The :name property
is ignored.

There are three ways to specify the directory location. 

1. :full-name
2. :relative-to
3. the current directory (via *default-pathname-defaults*)

In all cases, the report will go into 

|#

(defun handle-configuration-problem (class message &rest args)
  (let ((msg (apply #'format nil message args)))
    (accumulate-problem (make-instance class :message msg)
			*test-result*)
    (format *error-output* "~&;;;;;;;;;;;;;;;;;;;;;;;;;;;;;~%~%")
    (format *error-output* "~a" msg)
    (format *error-output* "~%~%;;;;;;;;;;;;;;;;;;;;;;;;;;;;;~%~%")
    ))

(defgeneric generate-report-summary-pathname ()
  )

(defgeneric handle-config-preference (name args)
  )

(defgeneric validate-report-property (name args))

(defmethod validate-report-property (name args)
  (declare (ignore name args))
  t)

(defvar *current-configuration-stream* nil)

(defvar *current-asdf-system-name* nil
  "Holds the name of the system being tested when using the `:generic` 
configuration.

LIFT needs this to run the `:generic` configuration because this is
how it determines which configuration file to load. If you use 
`asdf:test-op` then this value will be set automatically. 
Otherwise, you will need to set it yourself.")

(eval-when (:load-toplevel :execute)
  (when (find-package :asdf)
    (defmethod asdf:perform :around ((operation asdf:test-op) (c asdf:system))
      (let ((*current-asdf-system-name* (asdf:component-name c)))
	(call-next-method)))))

(defun lift-relative-pathname (pathname &optional (errorp nil))
  "Merges pathname with either the path to the currently loading system
\(if there is one\) or the *default-pathname-defaults*."
  (let* ((asdf-package (find-package :asdf))
	 (srp-symbol (and asdf-package
			  (find-symbol (symbol-name 'system-relative-pathname) 
				       asdf-package)))
	 (srp (and *current-asdf-system-name* srp-symbol)))
    (labels ((try-it (path)
	       (let ((pathname (merge-pathnames pathname path)))
		 (if errorp (and pathname (probe-file pathname)) pathname))))
      (or (and srp (try-it (funcall srp *current-asdf-system-name* "")))
	  (try-it *default-pathname-defaults*)
	  (not errorp)
	  (and (not asdf-package)
	       (error "Unable to use :generic configuration option because ASDF is not loaded."))
	  (and (not srp-symbol)
	       (error "Unable to use :generic configuration option because asdf:system-relative-pathname is not function bound (maybe try updating ASDF?)"))
	  (and (not *current-asdf-system-name*)
	       (error "Unable to use :generic configuration option 
because the current system cannot be determined. You can either 
use asdf:test-op or bind *current-asdf-system-name* yourself."))))))

(defun find-generic-test-configuration (&optional (errorp nil))
  (flet ((try-it (path)
	   (and path (probe-file path))))
    (or (try-it (lift-relative-pathname "lift-local.config" errorp))
	(try-it (lift-relative-pathname "lift-standard.config" errorp))
	(and errorp
	     (error "Unable to use :generic configuration file neither lift-local.config nor lift-standard.config can be found.")))))

(defun report-summary-pathname ()
  (unique-filename (generate-report-summary-pathname)))

(defmethod generate-report-summary-pathname ()
  (lift-relative-pathname "test-results/summary.sav"))

#+(or)
(generate-report-summary-pathname)

(defvar *config-dribble-pathname* nil)

(defun run-tests-from-file (path)
  (let ((real-path (cond ((eq path :generic)
			  (setf path 
				(find-generic-test-configuration t)))
			 (t
			  (probe-file path)))))
    (unless real-path
      (error "Unable to find configuration file ~s" path)) 
    (setf *test-result*
	  (let* ((*package* *package*)
		 (*read-eval* nil)
		 (result (make-test-result path :multiple))
		 (*lift-dribble-pathname* nil)
		 (*config-dribble-pathname* nil)
		 (*lift-debug-output* *debug-io*)
		 (*lift-standard-output* *standard-output*)
		 (*test-break-on-errors?* nil)
		 (*test-run-subsuites?* t)
		 (*lift-equality-test* 'equal)
		 (*test-print-length* :follow-print)
		 (*test-print-level* :follow-print)
		 (*lift-if-dribble-exists* :append)
		 (*test-result* result))
	    (%run-tests-from-file path)))))

(defun %run-tests-from-file (path)
  (with-open-file (*current-configuration-stream* path
      						  :direction :input
      						  :if-does-not-exist :error)
    (let ((form nil)
      	  (run-tests-p t)
	  (*lift-report-pathname* (report-summary-pathname)))
      (loop while (not (eq (setf form (read *current-configuration-stream* 
      					    nil :eof nil)) :eof)) 
      	 collect
	   (tagbody 
	      (flet ((stop-running-tests ()
		       (setf run-tests-p nil)))
		(declare (ignorable (function stop-running-tests)))
		#+allegro
		(excl:set-signal-handler excl::*sigterm*
				    (lambda (a b)
				      (declare (ignore a b))
				      (format t "~%Got sigterm~%")
				      (stop-running-tests)
				      (go :loop-end)))
		(handler-bind 
		    ((error 
		      (lambda (c)
			(handle-configuration-problem 
			 'test-configuration-error
			 "Error while running ~a from ~a: ~a" form path c)
					;(pprint (get-backtrace c))
			#+(or)
			;;
			(invoke-debugger c)
			)))
		  (format t "~&handle config: ~s" form)
		  (destructuring-bind
			(name &rest args)
		      form
		    (assert (typep name 'symbol) nil
			    "Each command must be a symbol and ~s is not." name)
		    (setf args (massage-arguments args))
		    (cond 
		      ;; check for preferences first (i.e., keywords)
		      ((eq (symbol-package name) 
			   (symbol-package :keyword))
		       ;; must be a preference
		       (handle-config-preference name args))
		      ((symbolp name)
		       (when run-tests-p
			 (multiple-value-bind (_ restartedp)
			     (restart-case 
				 (if (find-testsuite name :errorp nil)
				     (run-tests :suite name 
						:result *test-result*
						:dribble *config-dribble-pathname*
						:testsuite-initargs args)
				     (handle-configuration-problem
				      'test-configuration-failure
				      "Warning: testsuite ~s not found, skipping" name))
			       (cancel-testing-from-configuration (result)
				 :report (lambda (stream)
					   (format stream "Cancel testing from file ~a"
						   path))
				 (declare (ignore result))
				 (values nil t)))
			   (declare (ignore _))
			   ;; no more testing; continue to process commands
			   (when restartedp 
			     (setf run-tests-p nil)))))
		      (t
		       (handle-configuration-problem
			'test-configuration-failure "Don't understand '~s' while reading from ~s" 
			form path))))))
	      :loop-end))))
  (values *test-result*))

(defun massage-arguments (args)
  (loop for arg in args collect
       (cond ((and (symbolp arg)
		   (string= (symbol-name arg) (symbol-name '*standard-output*)))
	      *standard-output*)
	     (t arg))))

(defmethod handle-config-preference ((name t) args)
  (handle-configuration-problem 'test-configuration-failure "Unknown preference ~s (with arguments ~s)" 
		     name args))

(defmethod handle-config-preference ((name (eql :include)) args)
  (%run-tests-from-file (merge-pathnames (ensure-string (first args))
					 *current-configuration-stream*)))

(defconfig-variable :log-tag *log-tag*)

(defconfig-variable :dribble *config-dribble-pathname*)

(defconfig-variable :debug-output *lift-debug-output*)

(defconfig-variable :standard-output *lift-standard-output*)

(defconfig-variable :break-on-errors? *test-break-on-errors?*)

(defconfig-variable :do-children? *test-run-subsuites?*)

(defconfig-variable :equality-test *lift-equality-test*)

(defconfig-variable :print-length *test-print-length*)

(defconfig-variable :print-level *test-print-level*)

(defconfig-variable :print-suite-names *test-print-testsuite-names*)

(defconfig-variable :print-test-case-names *test-print-test-case-names*)

(defconfig-variable :if-dribble-exists *lift-if-dribble-exists*)

(defmethod handle-config-preference ((name (eql :report-property))
				     args)
  (let ((name (form-keyword (first args))))
    (validate-report-property name (second args))
    (setf (test-result-property *test-result* name) (second args))))

(defmethod validate-report-property ((name (eql :name)) args)
  ;; report name
  (assert (stringp args) nil
	  ":name property must be given a string, was given ~s" args)
  (assert (null (pathname-directory args)) nil
	  "The :name property must be a simple name (not a directory), use :full-pathname if
you need more control"))

(defconfig-variable :profiling-threshold *profiling-threshold*)

(defconfig-variable :count-calls-p *count-calls-p*)

(defconfig-variable :log-pathname *lift-report-pathname*)

(defconfig-variable :maximum-failures *test-maximum-failure-count*)
(defconfig-variable :maximum-failure-count *test-maximum-failure-count*)

(defconfig-variable :maximum-errors *test-maximum-error-count*)
(defconfig-variable :maximum-error-count *test-maximum-error-count*)

(defconfig-variable :profile-style *profile-style*)

(defgeneric report-pathname (method &optional result))

(defmethod report-pathname :around ((method (eql :html)) 
				    &optional (result *test-result*))
  (cond ((and (test-result-property result :full-pathname)
	      (streamp (test-result-property result :full-pathname)))
	 (call-next-method))
	(t
	 (let ((old-name (test-result-property result :name))
	       (old-full-pathname (test-result-property result :full-pathname))
	       (old-unique-name (test-result-property result :unique-name)))
	   (unwind-protect
		(progn
		  (setf (test-result-property result :name) t
			(test-result-property result :unique-name) nil)
		  (let ((destination (pathname-sans-name+type (call-next-method))))
		    (when old-name
		      (setf destination
			    (merge-pathnames
			     (make-pathname :directory `(:relative ,old-name))
			     destination)))
		    (print destination)
		    (merge-pathnames
		     (make-pathname :name "index" :type "html")
		     (pathname-sans-name+type
		      (if old-unique-name 
			  (unique-directory destination)
			  destination)))))
	     (setf (test-result-property result :name) old-name
		   (test-result-property result :full-pathname) 
		   old-full-pathname
		   (test-result-property result :unique-name) 
		   old-unique-name))))))

#+(or)
(defmethod report-pathname :around ((method t) &optional (result *test-result*))
  "Make sure that directories exist"
  (let ((output (call-next-method)))
    (cond ((streamp output)
	   output)
	  (t
	   (ensure-directories-exist output)
	   output))))

(defmethod report-pathname ((method t) &optional (result *test-result*))
  (let* ((given-report-name (test-result-property result :name))
	 (report-type (string-downcase
		       (ensure-string 
			(test-result-property result :format))))
	 (report-name (or (and given-report-name
			       (not (eq given-report-name t))
			       (merge-pathnames
				given-report-name 
				(make-pathname :type report-type)))
			  (format nil "report.~a" report-type)))
	 (via nil)
	 (dest (or (and (setf via :full-pathname)
			(test-result-property result :full-pathname)
			(streamp
			 (test-result-property result :full-pathname))
			(test-result-property result :full-pathname))
		   (and (setf via :full-pathname)
			(test-result-property result :full-pathname)
			(not (streamp
			      (test-result-property result :full-pathname)))
			(cond ((eq given-report-name t)
			       (test-result-property result :full-pathname))
			      ((null given-report-name)
			       (merge-pathnames
				(test-result-property result :full-pathname)
				report-name))
			      (t
			       (merge-pathnames
				(test-result-property result :full-pathname)
				given-report-name))))
		   (and (setf via :relative-to)
			(let ((relative-to 
			       (test-result-property result :relative-to)))
			  (and relative-to
			       (asdf:find-system relative-to nil)
			       (asdf:system-relative-pathname 
				relative-to report-name))))
		   (and (setf via :current-directory)
			(merge-pathnames
			 (make-pathname :defaults report-name)))))
	 (unique-name? (test-result-property result :unique-name)))
    (when (stringp dest)
      (setf dest (translate-user dest)))
    (values 
     (if (and unique-name? (not (streamp dest)))
	 (unique-filename dest)
	 dest)
     via)))

(defun translate-user (dest)
  (loop for position = (search "$user" dest :test #'char-equal) 
     while position do
     (setf dest (concatenate 
		 'string (subseq dest 0 position) (current-user)
		 (subseq dest (+ position (length "$user"))))))
  dest)

#+(or)
(translate-user "/usr/home/foo/$user/bar")

#+(or)
(translate-user "/usr/home/foo/$user/bar/$user-foo")


(defmethod handle-config-preference ((name (eql :build-report))
				     args)
  (declare (ignore args))
  (let* ((format (or (test-result-property *test-result* :format)
		     :html))
	 (dest (report-pathname format *test-result*)))
    (with-standard-io-syntax 
      (let ((*print-readably* nil))
	(handler-bind 
	    ((error 
	      (lambda (c)
		(format *debug-io*
			"Error ~a while generating report (format ~s) to ~a"
			c format dest)
		(format *debug-io*
			"~%~%Backtrace~%~%~s" 
			(get-backtrace c)))))
	  (cond
	    ((or (streamp dest) 
		 (ensure-directories-exist dest)
		 (writable-directory-p dest))
	     (format *debug-io* "~&Sending report (format ~s) to ~a" 
		     format dest)
	     (loop for hook in (report-hooks-for :report-display-name) do
	       (funcall hook format dest))
	     (test-result-report
	      *test-result* dest format))
	    (t
	     (format *debug-io* "~&Unable to write report (format ~s) to ~a" 
		     format dest))))))))
  

(defconfig :trace 
  "Start tracing each of the arguments to :trace."
  (eval `(trace ,@args)))

(defconfig :untrace
  (eval `(untrace ,@args)))

(defconfig :skip-tests-reset
  (setf *skip-tests* nil))

(defconfig :skip-testsuites
  (loop for arg in args do
       (if (find-testsuite arg)
	   (push arg *skip-tests*)
	   (handle-configuration-problem 'test-configuration-failure "Unable to find testsuite ~a to skip" arg))))

(defconfig :skip-tests 
  (loop for arg in args do
       (let ((suite (if (consp arg) (first arg) arg))
	     (test-case (if (consp arg) (second arg) nil)))
	 (cond ((not (or (atom arg) 
			 (= (length arg) 1) (= (length arg) 2)))
		(handle-configuration-problem 
		 'test-configuration-failure
		 ":skip-tests takes atoms or two element lists as arguments. Ignoring ~a in ~a" 
		 arg args))
	       ((and (null test-case) (null (find-testsuite suite)))
		(handle-configuration-problem
		 'test-configuration-failure
		 "Unable to find testsuite ~a to skip" suite))
	       ((and test-case (null (find-test-case suite test-case)))
		(handle-configuration-problem 
		 'test-configuration-failure
		 "Unable to find test-case ~a in testsuite ~a to skip" 
		 test-case suite))
	       (t
		(push (list suite test-case) *skip-tests*))))))

(defun current-user ()
  #+allegro
  (or (ignore-errors (excl.osi:cuserid))
      (first (last (pathname-directory
		    (user-homedir-pathname)))))
  #-allegro
  (first (last (pathname-directory
		(user-homedir-pathname)))))
