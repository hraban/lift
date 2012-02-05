;;;-*- Mode: Lisp; Package: lift -*-

(in-package #:lift)

(defmethod accumulate-problem ((problem test-failure-mixin) result)
  (setf (failures result) (append (failures result) (list problem))))

(defmethod accumulate-problem ((problem testsuite-failure) result)
  (setf (failures result) (append (failures result) (list problem))))

(defmethod accumulate-problem ((problem test-expected-failure) result)
  (setf (expected-failures result) 
	(append (expected-failures result) (list problem))))

(defmethod accumulate-problem ((problem test-error-mixin) result)
  (setf (errors result) (append (errors result) (list problem))))

(defmethod accumulate-problem ((problem testsuite-error) result)
  (setf (errors result) (append (errors result) (list problem))))

(defmethod accumulate-problem ((problem test-serious-condition) result)
  (setf (errors result) (append (errors result) (list problem))))

(defmethod accumulate-problem ((problem testsuite-serious-condition) result)
  (setf (errors result) (append (errors result) (list problem))))

(defmethod accumulate-problem ((problem test-expected-error) result)
  (setf (expected-errors result) 
	(append (expected-errors result) (list problem))))

(defmethod accumulate-problem ((problem testcase-skipped) result)
  (setf (skipped-test-cases result) 
	(append (skipped-test-cases result) (list problem))))

(defmethod accumulate-problem ((problem testsuite-skipped) result)
  (setf (skipped-testsuites result) 
	(append (skipped-testsuites result) (list problem))))


;;; ---------------------------------------------------------------------------
;;; test conditions
;;; ---------------------------------------------------------------------------

(defcondition (lift-compile-error :exportp nil) (error)
  (msg)
  "Compile error: '~S'" msg)

(defcondition testsuite-not-defined (lift-compile-error)
  (testsuite-name)
  "Test class ~A not defined before it was used."
  testsuite-name)

(defcondition testsuite-ambiguous (lift-compile-error)
  (testsuite-name possible-matches)
  "There are several test suites named ~s: they are ~{~s~^, ~}"
  testsuite-name possible-matches)

(defcondition test-case-not-defined (lift-compile-error)
  (testsuite-name test-case-name)
  "Testsuite ~s has no test-case named ~s."
  testsuite-name test-case-name)

(defcondition test-case-ambiguous (lift-compile-error)
  (testsuite-name test-case-name possible-matches)
  "There are several test cases named ~s.~s: they are ~{~s~^, ~}"
  testsuite-name test-case-name possible-matches)

(defcondition test-condition (warning) 
  ((message :initform ""))
  "~%~A" message)

(defcondition (test-timeout-condition :slot-names (message)) (test-condition) 
  ((maximum-time :initform *test-maximum-time*))
  "Test ran out of time (longer than ~S-second~:P)" 
  maximum-time)

(defcondition (ensure-failed-error  :slot-names (message)) (test-condition) 
  ((assertion :initform ""))
   "Ensure failed: ~S ~@[(~a)~]" assertion message)

(defcondition (ensure-null-failed-error :slot-names (message)) (ensure-failed-error)
  ((value :initform "")
   (assertion :initform ""))
  "Ensure null failed: ~s evaluates to ~s ~@[(~a)~]"
  assertion value message)

(defcondition ensure-expected-condition (test-condition) 
  ((expected-condition-type
    :initform nil)
   (the-condition
    :initform nil))
  "Expected ~S but got ~S~@[:~_   ~A~]" 
  expected-condition-type
  (type-of the-condition)
  (and (typep the-condition 'condition)
       the-condition))

(defcondition ensure-expected-no-warning-condition (test-condition) 
  ((the-condition
    :initform nil))
  "Expected no warnings but got ~S" 
  the-condition)

(defcondition failed-comparison-condition (test-condition) 
  (first-value second-value test))

(defcondition (ensure-not-same :slot-names (first-value second-value test message))
    (failed-comparison-condition) 
  ()
  "Ensure-same: ~S is not ~a to ~S~@[ (~a)~]"
  first-value (test-function-name test)
  second-value message)

(defcondition (ensure-same :slot-names (first-value second-value test message))
    (failed-comparison-condition) 
  ()
  "Ensure-different: ~S is ~a to ~S~@[ (~a)~]"
  first-value (test-function-name test) second-value message)

(defcondition ensure-cases-failure (test-condition)
  ((total :initform 0)
   (problems :initform nil)
   (errors :initform nil))
  (format 
   stream 
   "Ensure-cases: ~d case~:p with ~[~:;~:*~d error~:p; ~]~[~:;~:*~d failure~:p; ~]"
   total (length errors) (length problems))
  (when errors
    (format stream "~&Errors: ~@<  ~@;~{~%  ~{~20s ~3,8@t~a~}~^, ~}~:>" 
	    errors))
  (when problems
    (format stream "~&Failures: ~@<  ~@;~{~%  ~{~20s ~3,8@t~a~}~^, ~}~:>" 
	    problems)))

(defcondition unexpected-success-failure (test-condition)
  (expected expected-more)
  "Test succeeded but we expected ~s (~s)"
  expected expected-more)

(defun build-lift-error-message (context message &rest arguments)
  (format nil "~A: ~A" 
          context
          (apply #'format nil message arguments)))

(defun signal-lift-error (context message &rest arguments)
  (let ((c (make-condition  
            'lift-compile-error
            :msg (apply #'build-lift-error-message
				 context message arguments))))
    (unless (signal c)
      (error c))))

(defun report-lift-error (context message &rest arguments)
  (format *debug-io* "~&~A."
          (apply #'build-lift-error-message context message arguments))
  (values))

(defun lift-report-condition (c)
  (format *debug-io* "~&~A." c))

(defun maybe-raise-not-same-condition (value-1 value-2 test 
				       report &rest arguments)
  (let ((condition (make-condition 'ensure-not-same 
                                   :first-value value-1
                                   :second-value value-2
                                   :test test
                                   :message (when report
                                              (apply #'format nil 
						     report arguments)))))
    (if (find-restart 'ensure-failed)
      (invoke-restart 'ensure-failed condition) 
      (warn condition))))

(defun maybe-raise-ensure-same-condition (value-1 value-2 test 
				       report &rest arguments)
  (let ((condition (make-condition 'ensure-same 
                                   :first-value value-1
                                   :second-value value-2
                                   :test test
                                   :message (when report
                                              (apply #'format nil 
						     report arguments)))))
    (if (find-restart 'ensure-failed)
      (invoke-restart 'ensure-failed condition) 
      (warn condition))))


;;; ---------------------------------------------------------------------------
;;; test-mixin
;;; ---------------------------------------------------------------------------

(defmethod testsuite-setup ((testsuite test-mixin) (result test-result))
  (values))

(defmethod testsuite-setup :before ((testsuite test-mixin) (result test-result))
  (push (type-of testsuite) (suites-run result))
  (setf (current-step result) :testsuite-setup))

(defmethod testsuite-expects-error ((testsuite test-mixin))
  nil)

(defmethod testsuite-expects-failure ((testsuite test-mixin))
  nil)

(defmethod testsuite-teardown ((testsuite test-mixin) (result test-result))
  ;; no-op
  )

(defmethod testsuite-teardown :after
    ((testsuite test-mixin) (result test-result))
  (setf (current-step result) :testsuite-teardown
	(real-end-time result) (get-internal-real-time)
	(real-end-time-universal result) (get-universal-time)))

;;;;

(defun canonize-skip-tests (skip-tests)
  (mapcar
   (lambda (datum)
     (cond ((or (atom datum)
		(and (= (length datum) 1)
		     (setf datum (first datum)))
		(and (= (length datum) 2) (null (second datum))
		     (setf datum (first datum))))
	    (cons (find-testsuite datum :errorp t) nil))
	   ((= (length datum) 2)
	    (cons (find-testsuite (first datum) :errorp t)
		  (or (and (keywordp (second datum)) (second datum))
		      (find-test-case (find-testsuite (first datum))
				      (second datum) :errorp t))))
	   (t
	    (warn "Unable to interpret skip datum ~a. Ignoring." 
		  datum))))
   skip-tests))

(defun test-result-property (result property &optional default)
  (getf (test-result-properties result) property default))

(defun (setf test-result-property) (value result property)
  (setf (getf (test-result-properties result) property) value))

(defmethod write-profile-information ((suite t))
  )

(defmethod equality-test ((suite test-mixin))
  #'equal)

(defmethod setup-test :before ((test test-mixin))
  (setf *test-scratchpad* nil))

(defmethod setup-test ((test test-mixin))
  (values))

(defmethod setup-test ((test symbol))
  (let ((*current-test* (make-testsuite test nil)))
    (setup-test *current-test*)
    *current-test*))

(defmethod test-case-teardown progn ((test test-mixin) (result test-result))
  (values))

(defmethod test-case-teardown :around ((test test-mixin) (result test-result))
  (setf (current-step result) :test-teardown)
  (call-next-method))

(defmethod initialize-instance :after ((testsuite test-mixin) &rest initargs 
				       &key &allow-other-keys)
  (declare (ignorable initargs))
  (when (null (testsuite-name testsuite))
    (setf (slot-value testsuite 'name) 
	  (symbol-name (type-of testsuite)))))

(defmethod print-object ((tc test-mixin) stream)
  (print-unreadable-object (tc stream :identity t :type t)
    (format stream "~a" (testsuite-name tc))))

;;; ---------------------------------------------------------------------------
;;; macros
;;; ---------------------------------------------------------------------------

(defun initialize-current-definition ()
  (setf *current-definition* nil))

(defun set-definition (name value)
  (let ((current (assoc name *current-definition*)))
    (if current
      (setf (cdr current) value)
      (push (cons name value) *current-definition*)))
  
  (values value))

(defstruct (code-block (:type list) (:conc-name nil))
  block-name (priority 0) filter code operate-when)

(defun add-code-block (name priority operate-when filter handler code)
  (let ((current (assoc name *code-blocks*))
        (value (make-code-block
                :operate-when operate-when
                :block-name name
                :priority priority
                :filter filter
                :code code)))
    (if current
      (setf (cdr current) value)
      (push (cons name value) *code-blocks*))  
    (eval 
     `(defmethod block-handler ((name (eql ',name)) value)
        (declare (ignorable value))
        ,@handler)))    
    (setf *code-blocks* (sort *code-blocks* #'< 
			      :key (lambda (name.cb)
				     (priority (cdr name.cb))))))

(defmacro deftest (testsuite-name superclasses slots &rest
                                  clauses-and-options) 
  "The `deftest` form is obsolete, see [deftestsuite][]."
  
  (warn "Deftest is obsolete, use deftestsuite instead.")
  `(deftestsuite ,testsuite-name ,superclasses ,slots ,@clauses-and-options))

(setf *code-blocks* nil)

(add-code-block
 :setup 1 :methods
 (lambda () t)
 '((setf (def :setup) (cleanup-parsed-parameter value)))
 'build-setup-test-method)

(add-code-block
 :teardown 100 :methods
 (lambda () (or (def :teardown) (def :direct-slot-names))) 
 '((setf (def :teardown) (cleanup-parsed-parameter value)))
 'build-test-teardown-method)

(add-code-block
 :function 0 :methods
 (lambda () (def :functions))
 '((push value (def :functions)))
 'build-test-local-functions)

(add-code-block
 :documentation 0 :class-def 
 nil 
 '((setf (def :documentation) (first value)))
 nil)

(add-code-block
 :export-p 0 :class-def
 nil 
 '((setf (def :export-p) (first value)))
 nil)

(add-code-block
 :export-slots 0 :class-def
 nil 
 '((setf (def :export-slots) (first value)))
 nil)

(add-code-block
 :run-setup 0 :class-def
 nil 
 '((push (first value) (def :default-initargs))
   (push :run-setup (def :default-initargs))
   (setf (def :run-setup) (first value)))
 'check-run-setup-value)

(defun %valid-run-setup-values ()
  '(:once-per-session :once-per-suite
    :once-per-test-case :never))

(defun check-run-setup-value ()
  (when (def :run-setup)
    (unless (member (def :run-setup) (%valid-run-setup-values))
      (error "The :run-setup option must be one of ~{~a~^, ~}." 
	     (%valid-run-setup-values)))))
    
(add-code-block
 :equality-test 0 :methods
 (lambda () (def :equality-test))
 '((setf (def :equality-test) (cleanup-parsed-parameter value)))
 'build-test-equality-test)

(add-code-block
 :expected-error 0 :methods
 (lambda () (def :expected-error))
 '((setf (def :expected-error) (cleanup-parsed-parameter value)))
 'build-testsuite-expected-error)

(add-code-block
 :expected-failure 0 :methods
 (lambda () (def :expected-failure))
 '((setf (def :expected-failure) (cleanup-parsed-parameter value)))
 'build-testsuite-expected-failure)

(add-code-block
 :log-file 0 :class-def
 nil 
 '((push (first value) (def :default-initargs))
   (push :log-file (def :default-initargs)))
 nil)

(add-code-block
 :dynamic-variables 0 :class-def
 nil 
 '((setf (def :direct-dynamic-variables) value))
 nil)

(add-code-block
 :categories 0 :class-def
 nil 
 '((push value (def :categories)))
 nil)

(add-code-block
 :default-initargs 1 :class-def
 (lambda () (def :default-initargs))
 '((dolist (x (reverse (cleanup-parsed-parameter value)))
   (push x (def :default-initargs))))
 nil)

(defmacro deftestsuite (testsuite-name superclasses slots &rest
			clauses-and-options) 
  "
Creates a testsuite named `testsuite-name` and, optionally, the code required for test setup, test tear-down and the actual test-cases. A testsuite is a collection of test-cases and other testsuites.

Test suites can have multiple superclasses (just like the classes that they are). Usually, these will be other test classes and the class hierarchy becomes the test case hierarchy. If necessary, however, non-testsuite classes can also be used as superclasses.

Slots are specified as in defclass with the following additions:

* Initargs and accessors are automatically defined. If a slot is named`my-slot`, then the initarg will be `:my-slot` and the accessors will be `my-slot` and `(setf my-slot)`. 
* If the second argument is not a CLOS slot option keyword, then it will be used as the `:initform` for the slot. I.e., if you have

        (deftestsuite my-test ()
          ((my-slot 23)))

    then `my-slot` will be initialized to 23 during test setup.

Test options are one of :setup, :teardown, :test, :tests, :documentation, :export-p, :dynamic-variables, :export-slots, :function, :categories, :run-setup, or :equality-test. 

* :categories - a list of symbols. Categories allow you to groups tests into clusters outside of the basic hierarchy. This provides finer grained control on selecting which tests to run. May be specified multiple times.

* :documentation - a string specifying any documentation for the test. Should only be specified once.

* :dynamic-variables - a list of atoms or pairs of the form (name value). These specify any special variables that should be bound in a let around the body of the test. The name should be symbol designating a special variable. The value (if supplied) will be bound to the variable. If the value is not supplied, the variable will be bound to nil. Should only be specified once.

* :equality-test - the name of the function to be used by default in calls to ensure-same and ensure-different. Should only be supplied once. 

* :export-p - If true, the testsuite name will be exported from the current package. Should only be specified once.

* :export-slots - if true, any slots specified in the test suite will be exported from the current package. Should only be specified once.

* :function - creates a locally accessible function for this test suite. May be specified multiple times. 

* :run-setup - specify when to run the setup code for this test suite. Allowed values are 

    * :once-per-test-case or t (the default)
    * :once-per-session
    * :once-per-suite
    * :never or nil

    :run-setup is handy when a testsuite has a time consuming setup phase that you do not want to repeat for every test.

* :setup - a list of forms to be evaluated before each test case is run.  Should only be specified once.

* :teardown - a list of forms to be evaluated after each test case is run. Should only be specified once.

* :test - Define a single test case. Can be specified multiple times.

* :tests - Define multiple test cases for this test suite. Can be specified multiple times.
"
  #+no-lift-tests
  `(values)
  #-no-lift-tests
  (let ((test-list nil)
        (options nil)
        (return (gensym)))
    ;; convert any clause like :setup foo into (:setup foo)
    (setf clauses-and-options 
          (convert-clauses-into-lists clauses-and-options *deftest-clauses*))
    (initialize-current-definition)
    (setf (def :testsuite-name) testsuite-name)
    (setf (def :superclasses) (mapcar (lambda (class) (find-testsuite class :errorp t))
				      superclasses))
    (setf (def :deftestsuite) t)
    ;; parse clauses into defs
    (loop for clause in clauses-and-options do
	 (typecase clause
	   (symbol (pushnew clause options))
	   (cons (destructuring-bind (kind &rest spec) clause
		   (case kind
		     (:test (push (first spec) test-list))
		     (:tests 
		      (loop for test in spec do
			   (push test test-list)))
		     (t (block-handler kind spec)))))
	   (t (error "When parsing ~S" clause))))
    (let ((slot-names nil) (slot-specs nil))
      (loop for slot in (if (listp slots) slots (list slots)) do 
	   (push (if (consp slot) (first slot) slot) slot-names)
	   (push (parse-brief-slot slot) slot-specs))
      (setf (def :slot-specs) (nreverse slot-specs)
            (def :direct-slot-names) (nreverse slot-names)
            (def :slots-parsed) t))
    ;;?? issue 27: breaks 'encapsulation' of code-block mechanism
    (setf (def :function-specs)
	  (loop for spec in (def :functions) collect
	       (destructuring-bind (name arglist &body body) (first spec)
		 (declare (ignore body))
		 `(,name ,arglist))))
    ;;?? needed
    (empty-test-tables testsuite-name)
    (compute-superclass-inheritence)
    (prog2
	(setf *testsuite-test-count* 0)
	`(eval-when (:compile-toplevel :load-toplevel :execute)
	   (eval-when (:compile-toplevel)
	     (push ',return *test-is-being-compiled?*))
	   (eval-when (:load-toplevel)
	     (push ',return *test-is-being-loaded?*))
	   (eval-when (:execute)
	     (push ',return *test-is-being-executed?*))
	   ;; remove previous methods (do this _before_ we define the class)
	   (unless (or *test-is-being-compiled?*
		       *test-is-being-loaded?*)
	     (remove-previous-definitions ',(def :testsuite-name)))
	   ,(build-test-class)
	   (unwind-protect
		(let ((*test-is-being-defined?* t))
		  (setf *last-test-case-name* nil)
		  (setf *last-testsuite-name* ',(def :testsuite-name)
			(test-slots ',(def :testsuite-name)) 
			',(def :slot-names)
			(testsuite-dynamic-variables ',(def :testsuite-name))
			',(def :dynamic-variables)
			;;?? issue 27: breaks 'encapsulation' of code-block
			;; mechanism
			(testsuite-function-specs ',(def :testsuite-name))
			',(def :function-specs))
		  ,@(when (def :export-p)
			  `((export '(,(def :testsuite-name)))))
		  ,@(when (def :export-slots?)
			  `((export ',(def :direct-slot-names))))
		  ;; make a place to save test-case information
		  (empty-test-tables ',(def :testsuite-name))
		  ;; create methods
		  ;; setup :before
		  ,@(loop for (nil . block) in *code-blocks* 
		       when (and block 
				 (code block)
				 (eq (operate-when block) :methods)
				 (or (not (filter block))
				     (funcall (filter block)))) collect
		       (funcall (code block)))
		  ,@(when (def :dynamic-variables)
			  `((defmethod do-testing :around
				((suite ,(def :testsuite-name)) result fn) 
			      (declare (ignore result fn))
			      (with-test-slots
				(cond ((done-dynamics? suite)
				       (call-next-method))
				      (t
				       (setf (slot-value suite 'done-dynamics?) t)
				       (let* (,@(def :dynamic-variables))
					 (declare (special 
						   ,@(mapcar 
						      #'car (def :dynamic-variables))))
					 (call-next-method))))))))
		  ;; tests
		  ,@(when test-list
			  `((let ((*test-evaluate-when-defined?* nil))
			      ,@(loop for test in (nreverse test-list) collect
				     `(addtest (,(def :testsuite-name)) 
					,@test))
			      (setf *testsuite-test-count* nil))))
		  ,(if (and test-list *test-evaluate-when-defined?*)
		       `(unless (or *test-is-being-compiled?*
				    *test-is-being-loaded?*)
			  (let ((*test-break-on-errors?* *test-break-on-errors?*))
			    (run-tests :suite ',testsuite-name)))
		       `(find-class ',testsuite-name)))
	     ;; cleanup
	     (setf *test-is-being-compiled?* 
		   (remove ',return *test-is-being-compiled?*))
	     (setf *test-is-being-loaded?* 
		   (remove ',return *test-is-being-loaded?*))
	     (setf *test-is-being-executed?* 
		   (remove ',return *test-is-being-executed?*)))))))
 
(defun compute-superclass-inheritence ()
  ;;?? issue 27: break encapsulation of code blocks
  ;;?? we assume that we won't have too deep a hierarchy or too many 
  ;; dv's or functions so that having lots of duplicate names is OK
  (let ((slots nil)
	(inherited-dynamic-variables nil)
	(dynamic-variables (%build-pairs (def :direct-dynamic-variables)))
	(function-specs nil))
    (dolist (super (def :superclasses))
      (cond ((find-testsuite super)
	     (setf slots (append slots (test-slots super))
		   inherited-dynamic-variables 
		   (append inherited-dynamic-variables 
			   (testsuite-dynamic-variables super))
		   function-specs
		   (append function-specs 
			   (testsuite-function-specs super))))
	    (t
	     (error 'testsuite-not-defined :testsuite-name super))))
    (loop for pair in inherited-dynamic-variables 
	 unless (find (first pair) dynamic-variables :key #'first) collect
	 (progn (push pair dynamic-variables) pair))
    (setf (def :slot-names) 
	  (remove-duplicates (append (def :direct-slot-names) slots))
	  (def :dynamic-variables) (nreverse dynamic-variables)
	  (def :function-specs)
	  (remove-duplicates 
	   (append (def :function-specs) function-specs)))
    (setf (def :superclasses)
	  (loop for class in (def :superclasses) 
	     unless (some (lambda (oter)
			    (and (not (eq class oter))
				 (member class (superclasses oter))))
			  (def :superclasses)) collect
	     class))))

(defun %build-pairs (putative-pairs)
  (let ((result nil))
    (dolist (putative-pair putative-pairs)
      (if (atom putative-pair)
        (push (list putative-pair nil) result)
        (push putative-pair result)))
    (nreverse result)))

(defmacro addtest (name &body test)
  "Adds a single new test-case to the most recently defined testsuite."
  #+no-lift-tests
  `nil
  #-no-lift-tests
  (let ((body nil)
	(return (gensym))
	(options nil) (documentation nil)
	(looks-like-suite-name (looks-like-suite-name-p name)))
    (cond (looks-like-suite-name
	   ;; testsuite given
	   (setf (def :testsuite-name) (first name) 
		 options (rest name)
		 name nil body test))
	  (t
	   ;; the 'name' is really part of the test...
	   (setf body (cons name test))))
    (unless (property-list-p options)
      (signal-lift-error 'add-test "test-case options must be a property list and \"~s`\" is not" options)) 
    (when (getf options :documentation)
      (setf documentation (getf options :documentation))
      (remf options :documentation))
    (unless (def :testsuite-name)
      (when *last-testsuite-name*
	(setf (def :testsuite-name) *last-testsuite-name*)))
    (unless (def :testsuite-name)
      (signal-lift-error 'add-test +lift-no-current-test-class+))
    (unless (or (def :deftestsuite) 
		(find-testsuite (def :testsuite-name)))
      (signal-lift-error 'add-test +lift-test-class-not-found+
			 (def :testsuite-name)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (eval-when (:compile-toplevel)
	 (push ',return *test-is-being-compiled?*))
       (eval-when (:load-toplevel)
	 (push ',return *test-is-being-loaded?*))
       (eval-when (:execute)
	 (push ',return *test-is-being-executed?*))
       (unwind-protect
	    (let ((*test-is-being-defined?* t))
	      (muffle-redefinition-warnings
		,(build-test-test-method (def :testsuite-name) body options))
	      ,@(when documentation
		     `((setf (gethash 
			      ',(def :test-case-name)
			      (test-case-documentation ',(def :testsuite-name)))
			    ,documentation)))
	      (setf *last-testsuite-name* ',(def :testsuite-name))
	      (if *test-evaluate-when-defined?*
		  (unless (or *test-is-being-compiled?*
			      *test-is-being-loaded?*)
		    (let ((*test-break-on-errors?* (testing-interactively-p)))
		      (run-test)))
		  (values)))
	   ;; cleanup
	   (setf *test-is-being-compiled?* 
		 (remove ',return *test-is-being-compiled?*)
		 *test-is-being-loaded?*
		 (remove ',return *test-is-being-loaded?*)
		 *test-is-being-executed?*
		 (remove ',return *test-is-being-executed?*))))))

(defmacro addbenchmark ((suite-name &rest options) test-case-name &body body)
  "Adds a single new test-benchmark to testsuite suite-name."
  #+no-lift-tests
  `nil
  #-no-lift-tests
  (let ((documentation nil))
    (unless (property-list-p options)
      (signal-lift-error 
       'addbenchmark
       "benchmark options must be a property list and \"~s`\" is not" options)) 
    (when (getf options :documentation)
      (setf documentation (getf options :documentation))
      (remf options :documentation))
    (unless suite-name
      (signal-lift-error 'addbenchmark +lift-no-current-test-class+))
    (unless (find-testsuite suite-name)
      (signal-lift-error 
       'addbenchmark +lift-test-class-not-found+ suite-name))
    (setf (def :testsuite-name) suite-name
	  (def :test-case-name) test-case-name)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (let ((*test-is-being-defined?* t))
	 (muffle-redefinition-warnings
	   ,(build-benchmark-function
	     suite-name test-case-name body options))
	 ,@(when documentation
		 `((setf (gethash 
			  ',(def :test-case-name)
			  (test-case-documentation ',(def :testsuite-name)))
			 ,documentation)))
	 (setf *last-testsuite-name* ',(def :testsuite-name))
	 ',(def :test-case-name)))))

(defun looks-like-suite-name-p (form)
  (and (consp form)
       (atom (first form))
       (find-testsuite (first form))))

(defun property-list-p (form)
  (and (listp form)
       (block check-it
	 (let ((even? t))
	   (loop for x in form 
	      for want-keyword? = t then (not want-keyword?) do
		(when (and want-keyword? (not (keywordp x)))
		  (return-from check-it nil))
		(setf even? (not even?)))
	   (return-from check-it even?)))))

#|
(property-list-p '(:a :b))
(property-list-p '(:a 2 :b 3 :c 5 :d 8))
(property-list-p nil)

(property-list-p 3)
(property-list-p '(3))
(property-list-p '(3 :a))
(property-list-p '(:a 3 :b))
|#

(defun remove-test (&key (test-case *last-test-case-name*)
                         (suite *last-testsuite-name*))
  (assert suite nil "Test suite could not be determined.")
  (assert test-case nil "Test-case could not be determined.")
  (setf (testsuite-tests suite)
	(remove test-case (testsuite-tests suite))))

(defun make-testsuite (suite-name args)
  (let ((testsuite (find-testsuite suite-name :errorp t))
	result)
    (if testsuite
	(setf result (apply #'make-instance testsuite args))
	(error "Testsuite ~a not found." suite-name))
    result))

(defun skip-test-case-p (result suite-name test-case-name)
  (declare (ignore result))
  (find-if (lambda (skip-datum)
	     (if (cdr skip-datum)
		 (and (eq suite-name (car skip-datum))
		      (eq test-case-name (cdr skip-datum)))
		 (subtypep suite-name (car skip-datum))))
	   *skip-tests*))

(defun skip-test-suite-children-p (result suite-name)
  (declare (ignore result))
  (find-if (lambda (skip-datum)
	     (and (subtypep suite-name (car skip-datum))
		  (null (cdr skip-datum))))
	   *skip-tests*))

(defmethod skip-test-case (result suite-name test-case-name)
  (report-test-problem 'testcase-skipped result suite-name test-case-name nil))

(defmethod skip-testsuite (result suite-name)
  (report-test-problem 'testsuite-skipped result suite-name nil nil))

(defun test-case-expects-error-p (suite-name test-case-name)
  (or (testsuite-expects-error *current-test*)
      (test-case-option suite-name test-case-name :expected-error)))

(defun test-case-expects-failure-p (suite-name test-case-name)
  (or (testsuite-expects-failure *current-test*)
      (test-case-option suite-name test-case-name :expected-failure)))

(defun test-case-expects-problem-p (suite-name test-case-name)
  (test-case-option suite-name test-case-name :expected-problem))

(defun check-for-surprises (suite-name test-case-name)
  (let* ((expected-failure-p (test-case-expects-failure-p 
			      suite-name test-case-name))
	 (expected-error-p (test-case-expects-error-p 
			    suite-name test-case-name))
	 (expected-problem-p (test-case-expects-problem-p 
			      suite-name test-case-name))
	 (condition nil))
    (cond
      (expected-failure-p
       (setf condition 
	     (make-condition 'unexpected-success-failure
			     :expected :failure
			     :expected-more expected-failure-p)))
      (expected-error-p
       (setf condition 
	     (make-condition 'unexpected-success-failure
			     :expected :error
			     :expected-more expected-error-p)))
      (expected-problem-p
       (setf condition 
	     (make-condition 'unexpected-success-failure
			     :expected :problem
			     :expected-more expected-problem-p))))
    (when condition
      (if (find-restart 'ensure-failed)
	  (invoke-restart 'ensure-failed condition)
	  (warn condition)))))

(defun report-test-problem (problem-type result suite-name method condition
			    &rest args)
  ;; ick
  (let ((docs nil)
	(option nil))
    (declare (ignorable docs option))
    (cond ((and (eq problem-type 'test-failure)
		(not (typep condition 'unexpected-success-failure))
		(test-case-expects-failure-p suite-name method))
	   (setf problem-type 'test-expected-failure 
		 option :expected-failure))
	  ((and (eq problem-type 'test-error)
		(test-case-expects-error-p suite-name method))
	   (setf problem-type 'test-expected-error
		 option :expected-error))
	  ((and (or (eq problem-type 'test-failure) 
		    (eq problem-type 'test-error))
		(test-case-expects-problem-p suite-name method))
	   (setf problem-type (or (and (eq problem-type 'test-failure) 
				       'test-expected-failure)
				  (and (eq problem-type 'test-error)
				       'test-expected-error))
		 option :expected-problem)))
    (let ((problem (apply #'make-instance problem-type
			  :testsuite suite-name
			  :test-method method 
			  :test-condition condition
			  :test-step (current-step result)
			  :testsuite-initargs (testsuite-initargs result) 
			  args)))
      (when *current-test*
	(setf (getf (test-data *current-test*) :problem) problem))
      (accumulate-problem problem result)
      (when (and *test-maximum-failure-count*
		 (numberp *test-maximum-failure-count*)
		 (>= (length (failures result)) *test-maximum-failure-count*))
	(cancel-testing :failures))
      (when (and *test-maximum-error-count*
		 (numberp *test-maximum-error-count*)
		 (>= (length (errors result)) *test-maximum-error-count*))
	(cancel-testing :errors))
      problem)))

(defun cancel-testing (why)
  (declare (ignore why))
  (flet ((do-it (name)
	   (let ((restart (find-restart name)))
	     (when restart (invoke-restart restart *test-result*)))))
    (do-it 'cancel-testing-from-configuration)
    (do-it 'cancel-testing)))

;;; ---------------------------------------------------------------------------
;;; test-result and printing
;;; ---------------------------------------------------------------------------

(defun get-test-print-length ()
  (let ((foo *test-print-length*))
    (if (eq foo :follow-print) *print-length* foo)))

(defun get-test-print-level ()
  (let ((foo *test-print-level*))
    (if (eq foo :follow-print) *print-level* foo)))

(defun record-start-times (result suite) 
  (setf (current-step result) :start-test
	(test-data suite) 
	`(:start-time ,(get-internal-real-time)
	  :start-time-universal ,(get-universal-time))))

(defun record-end-times (result suite)
  (setf (current-step result) :end-test
	(getf (test-data suite) :end-time) (get-internal-real-time)
	(end-time result) (get-internal-real-time)
	(getf (test-data suite) :end-time-universal) (get-universal-time)
	(end-time-universal result) (get-universal-time)))

(defmethod make-test-result (for test-mode &rest args)
  (apply #'make-instance 'test-result
	 :results-for for
	 :test-mode test-mode 
	 args))

(defun testing-interactively-p ()
  (values nil))

(defmethod print-object ((tr test-result) stream)
  (let ((complete-success? (and (null (errors tr))
                                (null (failures tr))
				(null (expected-failures tr))
				(null (expected-errors tr)))))
    (let* ((*print-level* (get-test-print-level))
           (*print-length* (get-test-print-length))
	   (non-failure-failures
	    (count-if 
	     (lambda (failure) 
	       (member (class-of (test-condition failure))
		       (subclasses 'unexpected-success-failure :proper? nil)))
	     (expected-failures tr)))
	   (expected-failures (- (length (expected-failures tr))
				 non-failure-failures)))	     
      (print-unreadable-object (tr stream)
        (cond ((and (null (tests-run tr)) complete-success?)
               (format stream "~A: no tests run" (results-for tr)))
              ((eq (test-mode tr) :single)
               (cond ((test-interactive? tr)
                      ;; interactive
                      (cond (complete-success?
                             (format stream "Test passed"))
                            ((errors tr)
                             (format stream "Error during testing"))
                            ((expected-errors tr)
                             (format stream "Expected error during testing"))
                            ((failures tr)
                             (format stream "Test failed"))
			    ((plusp non-failure-failures)
                             (format stream "Test succeeded unexpectedly"))
                            (t
                             (format stream "Test failed expectedly"))))
                     (t
                      ;; from run-test
                      (format stream "~A.~A ~A" 
                              (results-for tr) 
                              (second (first (tests-run tr)))
                              (cond (complete-success?
                                     "passed")
                                    ((errors tr)
                                     "Error")
                                    (t
                                     "failed")))
		      (when (or (expected-errors tr) (expected-failures tr))
			(format stream "(~[~:;, ~:*~A expected failure~:P~]~[~:;, ~:*~A succeeded unexpectedly~]~[~:;, ~:*~A expected error~:P~])" 
				expected-failures non-failure-failures
				(expected-errors tr))))))
              (t
               ;; multiple tests run
               (format stream "Results for ~A " (results-for tr))
               (if complete-success?
                 (format stream "[~A Successful test~:P]"
                         (length (tests-run tr)))
                 (format stream "~A Test~:P~[~:;, ~:*~A Failure~:P~]~[~:;, ~:*~A Error~:P~]~[~:;, ~:*~A Expected failure~:P~]~[~:;, ~:*~A Expected error~:P~]" 
                         (length (tests-run tr))
                         (length (failures tr))
                         (length (errors tr))
                         (length (expected-failures tr))
                         (length (expected-errors tr))))))
        ;; note that suites with no tests think that they are completely 
        ;; successful. Optimistic little buggers, huh?
        (when (and (not complete-success?) *test-describe-if-not-successful?*)
          (format stream "~%") 
          (print-test-result-details stream tr t t))))))

(defmethod describe-object ((result test-result) stream)
  (describe-test-result result stream))

(defmethod describe-test-result (result stream 
				 &key
				 (show-details-p *test-show-details-p*)
				 (show-expected-p *test-show-expected-p*)
				 (show-code-p *test-show-code-p*))
  (let* ((number-of-failures (length (failures result)))
	 (number-of-errors (length (errors result)))
	 (number-of-expected-errors (length (expected-errors result)))
	 (non-failure-failures
	  (count-if 
	   (lambda (failure) 
	     (member (class-of (test-condition failure))
		     (subclasses 'unexpected-success-failure :proper? nil)))
	   (expected-failures result)))
	 (number-of-expected-failures (- (length (expected-failures result))
					 non-failure-failures))
	 (*print-level* (get-test-print-level))
	 (*print-length* (get-test-print-length)))
    (unless *test-is-being-defined?*
      (print-test-summary result stream)
      (when (and show-details-p
		 (or (plusp number-of-failures)
                     (plusp number-of-expected-failures)
	             (plusp number-of-errors)
                     (plusp number-of-expected-errors)))
	(format stream "~%~%")             
	(print-test-result-details
	 stream result show-expected-p show-code-p)
	(print-test-summary result stream)))))

(defun print-test-summary (result stream)
  (let* ((number-of-failures (length (failures result)))
	 (number-of-errors (length (errors result)))
	 (number-of-expected-errors (length (expected-errors result)))
	 (non-failure-failures
	  (count-if 
	   (lambda (failure) 
	     (member (class-of (test-condition failure))
		     (subclasses 'unexpected-success-failure :proper? nil)))
	   (expected-failures result)))
	 (number-of-expected-failures (- (length (expected-failures result))
					 non-failure-failures)))	     
    (format stream "~&Test Report for ~A: ~D test~:P run" 
	    (results-for result) (length (tests-run result)))
    (cond ((or (failures result) (errors result)
	       (expected-failures result) (expected-errors result))
	   (format stream "~[~:;, ~:*~A Error~:P~]~[~:;, ~:*~A Failure~:P~]~[~:;, ~:*~A Expected error~:P~]~[~:;, ~:*~A Expected failure~:P~]~[~:;, ~:*~A Successful Surprise~:P~]." 
		   number-of-errors
		   number-of-failures
		   number-of-expected-errors
		   number-of-expected-failures
		   non-failure-failures))
	  ((or (expected-failures result) (expected-errors result))
	   (format stream ", all passed *~[~:;, ~:*~A Expected error~:P~]~[~:;, ~:*~A Expected failure~:P~])." 
		   number-of-expected-errors
		   number-of-expected-failures))
	  (t
	   (format stream ", all passed!")))))

(defun print-test-result-details (stream result show-expected-p show-code-p)
  (loop for report in (errors result) do
       (print-test-problem "ERROR  :" report stream
			   show-code-p))  
  (loop for report in (failures result) do
       (print-test-problem "Failure:" report stream 
			   show-code-p))
  (when show-expected-p
    (loop for report in (expected-failures result) do
	 (print-test-problem "Expected failure:" report stream
			     show-code-p))
    (loop for report in (expected-errors result) do
	 (print-test-problem "Expected Error :" report stream
			     show-code-p))))

(defmethod print-test-problem (prefix (report testsuite-problem-mixin) stream show-code-p)
  (let* ((suite-name (testsuite report))
         (method (test-method report))
         (condition (test-condition report))
         (code (test-report-code suite-name method))
	 (step (test-step report))
         (testsuite-name method)	 
	 (*print-level* (get-test-print-level))
	 (*print-length* (get-test-print-length)))
    (let ((*package* (symbol-package method))
	  (doc-string (gethash testsuite-name
			       (test-case-documentation suite-name))))
      (format stream "~&~A ~(~A : ~A~)" prefix suite-name testsuite-name)
      (if show-code-p
	  (setf code (with-output-to-string (out)
		       (pprint code out)))
	  (setf code nil))
      (format stream "~&~<  ~@;~
                    ~@[Documentation: ~<~@;~a~:>~]~
                    ~@[~&Condition    : ~<~@;~a~:>~]~
                    ~@[~&During       : ~a~]~
                    ~@[~&Code         : ~a~]~
                    ~&~:>" (list doc-string (list condition) step code)))))

(defmethod print-test-problem (prefix (report test-configuration-problem-mixin) stream show-code-p)
  (declare (ignore show-code-p))
  (format stream "~&~A ~a~%~%" prefix (test-problem-message report)))


;;; ---------------------------------------------------------------------------
;;; test-reports
;;; ---------------------------------------------------------------------------

(defun test-report-code (suite-name test-case-name)
  (gethash test-case-name (test-name->code-table suite-name)))

;;; ---------------------------------------------------------------------------
;;; utilities
;;; ---------------------------------------------------------------------------

(defun remove-test-methods (test-name)
  (prog1
      (length (testsuite-tests test-name))
    (setf (testsuite-tests test-name) nil)))

(defun remove-previous-definitions (classname)
  "Remove the methods of this class and all its subclasses."
  (let ((classes-removed nil)
        (class (find-class classname nil))
        (removed-count 0))
    (when class
      (loop for subclass in (subclasses class :proper? nil) do
            (push subclass classes-removed)
            (incf removed-count
                  (remove-test-methods (class-name subclass)))
            #+Ignore
            ;;?? causing more trouble than it solves...??
            (setf (find-class (class-name subclass)) nil))
      
      (unless (length-1-list-p classes-removed)
        (format *debug-io* 
                "~&;;; Removed Test suite ~(~A~) and its subclasses (~{~<~s~>~^, ~})."
                classname (sort 
                           (delete classname 
				   (mapcar #'class-name classes-removed))
                           #'string-lessp)))
      (unless (zerop removed-count)
        (format *debug-io* 
                "~&;;; Removed ~D methods from test suite ~(~A~)~@[ and its subclasses~]."
                removed-count classname 
		(not (length-1-list-p classes-removed)))))))

(defun (setf test-environment-value) (value name)
  (setf (slot-value *current-test* name) value))

(defun test-environment-value (name)
  (slot-value *current-test* name))

(defun build-test-local-functions ()
  `(progn
     ,@(mapcar 
	(lambda (function-spec)
	  (destructuring-bind (name arglist &body body) (first function-spec)
	    `(defmethod flet-test-function ((testsuite ,(def :testsuite-name))
					    (function-name (eql ',name))
					    &rest args)
	       (with-test-slots 
		 ,(if arglist
		      `(destructuring-bind ,arglist args
			 ,@body)
		      `(progn ,@body))))))
	(def :functions))))

(defun build-test-equality-test ()
  (let ((test-name (def :testsuite-name))
        (equality-test (def :equality-test)))
    `(progn
       (defmethod equality-test ((testsuite ,test-name))
	 ,equality-test))))

(defun build-testsuite-expected-error ()
  (let ((test-name (def :testsuite-name))
        (expected-error (def :expected-error)))
    `(progn
       (defmethod testsuite-expects-error ((testsuite ,test-name))
	 (with-test-slots
	   ,expected-error)))))

(defun build-testsuite-expected-failure ()
  (let ((test-name (def :testsuite-name))
        (expected-failure (def :expected-failure)))
    `(progn
       (defmethod testsuite-expects-failure ((testsuite ,test-name))
	 (with-test-slots
	   ,expected-failure)))))

(defun build-test-teardown-method ()
  (let ((test-name (def :testsuite-name))
        (teardown (def :teardown)))
    (when teardown
      (unless (consp teardown)
        (setf teardown (list teardown)))
      (when (length-1-list-p teardown)
        (setf teardown (list teardown)))
      (when (symbolp (first teardown))
        (setf teardown (list teardown))))
    (let* ((teardown-code `(,@(when teardown
                                `((with-test-slots ,@teardown)))))
           (test-code `(,@teardown-code)))
      `(progn
         ,@(when teardown-code
             `((defmethod test-case-teardown progn ((testsuite ,test-name)
						    (result test-result))
		 (when (run-teardown-p testsuite :test-case)
		   ,@test-code))))
         ,@(when teardown-code
             `((defmethod testsuite-teardown ((testsuite ,test-name)
					      (result test-result))
                 (when (run-teardown-p testsuite :testsuite)
		   ,@test-code))))))))

(defun build-setup-test-method ()
  (let ((test-name (def :testsuite-name))
        (setup (def :setup)))
    ;;?? ewww, this smells bad
    (when setup
      (unless (consp setup)
        (setf setup (list setup)))
      (when (length-1-list-p setup)
        (setf setup (list setup)))
      (when (symbolp (first setup))
        (setf setup (list setup))))
    (if setup
      `(defmethod setup-test :after ((testsuite ,test-name))
	 (with-test-slots
	   ,@setup))
      ;; rather use remove-method
      `(defmethod setup-test :after ((testsuite ,test-name))
	 ))))    

(defmethod setup-test :around ((test test-mixin))
  (when (run-setup-p test)
    (call-next-method)
    (setf (slot-value test 'done-setup?) t)))

(defun run-setup-p (testsuite)
  (case (run-setup testsuite)
    (:once-per-session (error "not implemented"))
    (:once-per-suite (not (done-setup? testsuite)))
    ((:once-per-test-case t) t)
    ((:never nil) nil)
    (t (error "Don't know about ~s for run-setup" (run-setup testsuite)))))

(defun run-teardown-p (testsuite when)
  (ecase when
    (:test-case
     (ecase (run-setup testsuite)
       (:once-per-session nil)
       (:once-per-suite nil)
       ((:once-per-test-case t) t)
       ((:never nil) nil)))
    (:testsuite
     (ecase (run-setup testsuite)
       (:once-per-session nil)
       (:once-per-suite t)
       ((:once-per-test-case t) nil)
       ((:never nil) nil)))))
     
(defun build-test-test-method (suite-name test-body options)
  (multiple-value-bind (test-case-name body name-supplied?)
                       (parse-test-body test-body)
    (declare (ignorable name-supplied?))
    (unless (consp (first body))
      (setf body (list body)))
    (setf (def :test-case-name) test-case-name)
    `(progn
       (setf (gethash ',test-case-name (test-name->code-table ',suite-name)) ',body
             (gethash ',body (test-code->name-table ',suite-name)) ',test-case-name)
       #+(or mcl ccl)
       ,@(when name-supplied?
           `((ccl:record-source-file ',test-case-name 'test-case)))
       (unless (find ',test-case-name (testsuite-tests ',suite-name))
	 (setf (testsuite-tests ',suite-name)
	       (append (testsuite-tests ',suite-name) (list ',test-case-name))))
       ;;?? to defer until after compile...?
       ,@(when options
          `((defmethod set-test-case-options 
		((suite-name (eql ',suite-name)) (test-case-name (eql ',test-case-name)))
	      ,@(build-test-case-options 
		suite-name test-case-name options))))
       (setf (gethash ',test-case-name (test-name->methods ',suite-name))
	     (lambda (testsuite)
	       (declare (ignorable testsuite))
	       ,@(when options 
		       `((set-test-case-options ',suite-name ',test-case-name)))
	       (with-test-slots ,@body)))
       (setf *last-test-case-name* ',test-case-name)
       (when (and *test-print-when-defined?*
                  (not (or *test-is-being-compiled?*
                           )))
         (format *debug-io* "~&;Test Created: ~(~S.~S~)." 
		 ',suite-name ',test-case-name))
       *last-test-case-name*)))

(defun parse-test-body (test-body)
  (let ((test-name nil)
        (body nil)
        (test-number (1+ (testsuite-test-count *last-testsuite-name*)))
        (name-supplied? nil))
    (setf test-name (first test-body))
    (cond ((symbolp test-name)
           (setf test-name 
		 (intern (format nil "~A" test-name))
                 body (rest test-body)
                 name-supplied? t))
          ((and (test-code->name-table *last-testsuite-name*)
                (setf test-name 
                 (gethash test-body
			  (test-code->name-table *last-testsuite-name*))))
           (setf body test-body))
          (t
           (setf test-name 
		 (intern (format nil "TEST-~A" 
				 test-number))
                 body test-body)))
    (values test-name body name-supplied?)))

(defun build-benchmark-function (suite-name test-case-name body options)
  (let ((duration 2) style)
    (when (getf options :style)
      (setf style (getf options :style))
      (remf options :style))
    (when (getf options :duration 2)
      (setf duration (getf options :duration 2))
      (remf options :duration))
    `(progn
       #+(or mcl ccl)
       ,@(when name-supplied?
	       `((ccl:record-source-file ',test-case-name 'test-case)))
       (unless (find ',test-case-name (testsuite-tests ',suite-name))
	 (setf (testsuite-tests ',suite-name)
	       (append (testsuite-tests ',suite-name) (list ',test-case-name))))
       ;;?? to defer until after compile...?
       ,@(when options
	       `((defmethod set-test-case-options 
		     ((suite-name (eql ',suite-name))
		      (test-case-name (eql ',test-case-name)))
		   ,@(build-test-case-options 
		      suite-name test-case-name options))))
       (setf (gethash ',test-case-name (test-name->methods ',suite-name))
	     (lambda (testsuite)
	       (declare (ignorable testsuite))
	       (with-test-slots 
		 (symbol-macrolet 
		     ((benchmark-count 
		       (getf (test-data *current-test*) :benchmark-count)))
		   (declare (ignorable benchmark-count))
		   ,@(when options 
			   `((set-test-case-options ',suite-name ',test-case-name)))
		   ,@(ecase style
			    (:repetition
			     `((setf benchmark-count
				     (while-counting-repetitions (,duration)
				       ,@body))))
			    (:events
			     `((setf benchmark-count
				     (while-counting-events (,duration)
				       ,@body))))
			    ((nil)
			     `,body))))))
       (setf *last-test-case-name* ',test-case-name))))

(defun build-test-class ()
  ;; for now, we don't generate code from :class-def code-blocks
  ;; they are executed only for effect.
  (loop for (nil . block) in *code-blocks* 
     when (and block 
	       (code block)
	       (eq (operate-when block) :class-def)
	       (or (not (filter block))
		   (funcall (filter block)))) collect
     (funcall (code block)))
  (unless (some (lambda (superclass)
		  (testsuite-p superclass))
		(def :superclasses))
    (pushnew 'test-mixin (def :superclasses)))
  ;; build basic class and standard class
  `(defclass ,(def :testsuite-name) (,@(def :superclasses))
     ,(loop for name in (def :direct-slot-names) collect
	    (let ((it (find name (def :slot-specs) :key #'car)))
	      (assert it)
	      it))
     ,@(when (def :documentation)
	     `((:documentation ,(def :documentation))))
     (:default-initargs
       ,@(def :default-initargs))))

(defun parse-test-slots (slot-specs)
  (loop for spec in slot-specs collect
        (let ((parsed-spec spec))
          (if (member :initform parsed-spec)
            (let ((pos (position :initform parsed-spec)))
              (append (subseq parsed-spec 0 pos)
                      (subseq parsed-spec (+ pos 2))))
            parsed-spec))))

;;?? issue 27: break encapsulation of code blocks
(defclass-property testsuite-function-specs)

(defun empty-test-tables (test-name)
  (when (find-class test-name nil)
    (setf (test-code->name-table test-name)
          (make-hash-table :test #'equal)
          (test-name->code-table test-name)
          (make-hash-table :test #'equal)
          (test-name->methods test-name)
          (make-hash-table :test #'eq)
          (test-case-documentation test-name)
          (make-hash-table :test #'equal))))

(pushnew :timeout *deftest-clauses*)

(add-code-block
 :timeout 1 :class-def
 (lambda () (def :timeout)) 
 '((setf (def :timeout) (cleanup-parsed-parameter value)))
 (lambda ()
   (unless (some (lambda (super)
		   (member (find-class 'process-test-mixin)
			   (superclasses super)))
		 (def :superclasses))
     (pushnew 'process-test-mixin (def :superclasses)))
   (push (def :timeout) (def :default-initargs))
   (push :maximum-time (def :default-initargs))
   nil))

(defmethod do-test :around ((suite test-mixin) name result)
  (declare (ignore result))
  (if (profile suite)
      (with-profile-report ((format nil "~a-~a" 
				    (testsuite-name suite) name) 
			    (profile suite))
	(call-next-method))
      (call-next-method)))

(defmethod do-test :around ((suite process-test-mixin) name result)
  (declare (ignore name))
  (handler-bind ((timeout-error 
		  (lambda (c)
		    (let ((suite-name (class-name (class-of suite))))
		      (report-test-problem
		       'test-timeout-failure result suite-name (current-method suite)
		       (make-instance 'test-timeout-condition
				      :maximum-time (maximum-time suite))))
		    (if (find-restart 'test-failed)
			(invoke-restart 'test-failed c) 
			(error c)))))
    (with-timeout ((maximum-time suite))
      (call-next-method))
    ))

(defmethod testsuite-log-data ((suite t))
  nil)

(defmethod testsuite-log-data :around ((suite t))
  (multiple-value-bind (additional error?)
      (ignore-errors (call-next-method))
    (if error? 
	`(:error "error occured gathering additional data")
	additional)))

(defmethod test-case-teardown :around ((suite log-results-mixin) result)
  (declare (ignore result))
  (let ((problem (getf (test-data suite) :problem)))
    (unless (and problem (typep problem 'test-error-mixin))
      (generate-log-entry 
       nil
       (getf (test-data suite) :seconds)
       (getf (test-data suite) :conses)
       :additional-data 
       `(,@(testsuite-log-data suite))))))

;;?? might be "cleaner" with a macrolet (cf. lift-result)
(defun lift-property (name)
  (when *current-test*
    (getf (getf (test-data *current-test*) :properties) name)))

#+(or)
(setf (getf (getf (third (first (tests-run *test-result*))) :properties) :foo)
      3)

(defun (setf lift-property) (value name)
  (when *current-test*
    (setf (getf (getf (test-data *current-test*) :properties) name) value)))


#+Later
(defmacro with-test (&body forms)
  "Execute forms in the context of the current test class."
  (let* ((testsuite-name *last-testsuite-name*)
         (test-case (make-instance test-class)))
    `(eval-when (:execute)
       (prog2
        (setup-test ,test-case)
        (progn
          (with-test-slots ,@forms))
        (test-case-teardown ,test-case result)))))

(defvar *test-case-options* (make-hash-table))

(defun remove-test-case-options (suite-name)
  (remhash suite-name *test-case-options*))

(defun test-case-option (suite-name case-name option-name)
  (let* ((suite-options (gethash suite-name *test-case-options*))
	 (case-options (and suite-options 
			    (gethash case-name suite-options))))
    (getf (car case-options) option-name)))

(defun (setf test-case-option) (value suite-name case-name option-name)
  (let ((suite-options (gethash suite-name *test-case-options*)))
    (unless suite-options
      (setf suite-options (setf (gethash suite-name *test-case-options*)
				(make-hash-table))))
    (multiple-value-bind (case-options found?)
	(gethash case-name suite-options)
      (unless found?
	(setf case-options
	      (setf (gethash case-name suite-options) (cons nil nil))))
      (setf (getf (car case-options) option-name) value))))

(defun build-test-case-options (suite-name case-name options)
  (loop for (k v) on options by #'cddr collect
       `(setf (test-case-option ',suite-name ',case-name ,k) ,v)))

#|
(test-case-option 'test-dependencies-helper 'test-c :depends-on)
(setf (test-case-option 'test-dependencies-helper 'test-c :depends-on) :test-c)
(remove-test-case-options 'test-dependencies-helper)
|#
