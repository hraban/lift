(in-package #:lift)

(defvar *measures* nil
  "A list of defineded measures")

(defparameter *benchmark-log-path*
  (asdf:system-relative-pathname 
   'lift "benchmark-data/benchmarks.log"))

(defvar *count-calls-p* nil)

(defun compile-quickly (body)
  "Compile body with as much extra stuff as possible turned `off`. 

For example, compile without cross-reference information."
  (#-allegro let #+allegro excl::compiler-let 
	     (#+allegro (excl:*record-xref-info* nil))
     (compile nil body)))

(defmacro defclass-property (property &optional (default nil default-supplied?))
  "Create getter and setter methods for 'property' on symbol's property lists." 
  (let ((real-name (intern (format nil "~:@(~A~)" property) :keyword)))
    `(progn
       (defgeneric ,property (symbol))
       (defgeneric (setf ,property) (value symbol))
       (defmethod ,property ((class-name symbol))
          (get class-name ,real-name ,@(when default-supplied? (list default))))
       (defmethod (setf ,property) (value (class-name symbol))
         (setf (get class-name ,real-name) value)))))

(defmacro undefmeasure (name)
  (let ((gname (gensym "name-")))
    `(let ((,gname ,(form-keyword name)))
       (if (find ,gname *measures* :key #'first)
	   (setf *measures* (remove ,gname *measures* :key #'first))
	   (error "Measure ~a not found." ,gname))
       ,gname)))
  
(defmacro defmeasure (name &key (value nil) (finally nil) (type nil)
		      (documentation nil))
  (declare (ignore documentation))
  (unless value
    (error "A value must be specified to define a measure."))
  (cond ((atom name)
	 ;; all is well
	 )
	((and (eq (first name) 'quote)
	      (eq (length name) 2))
	 (warn "Name does not need to be quoted.")
	 (setf name (second name)))
	(t
	 (error "Name should be a symbol.")))
  (cond ((eq (first finally) 'quote)
	 (setf finally (second finally))))
  (let ((gname (gensym "name-")))
    `(let ((,gname ,(form-keyword name)))
       (setf *measures* (remove ,gname *measures* :key #'first))
       (push (list ,gname
		   :value ,value 
		   :finally ',finally
		   :type ',type)		   
	     *measures*)
       ,gname)))

(defmacro while-measuring ((catch-errors-p &rest measures) &body body)
  (let ((vars (loop for measure in measures collect
		   (gensym (format nil "~a-" measure))))
	(gcondition (gensym "condition-"))
	(gresult (gensym "result-"))
	(gcatch-errors-p (gensym "catch-errors-p-")))
    (labels ((measure-1 (vars measures)
	       (cond ((null measures) body)
		     (t
		      `((while-measuring-1 (,(first vars) ,(first measures))
			 ,@(measure-1 (rest vars) (rest measures))))))))
      `(let ((,gcondition nil)
	     (,gresult nil)
	     (,gcatch-errors-p ,catch-errors-p)
	     ,@vars)
	 (setf ,gresult
	       (handler-bind
		   ((error (lambda (c)
			     (setf ,gcondition c)
			     (unless ,gcatch-errors-p
			       (error c)))))
		 ,@(measure-1 vars measures)))
	 (values ,gresult (list ,@vars) ,gcondition)))))

#+(or) 
(while-measuring (space seconds)
    nil
  (sleep 1)
  (signal "hi"))

#+(or)
(measure-time-and-conses 
  (sleep 1)
  (signal "hi"))

(defmacro while-measuring-1 ((var measure) &body body)
  (let ((ginitial (gensym "value-"))
	(gresult (gensym "result-"))
	(metadata (find (form-keyword measure) *measures* :key 'first)))
    (unless metadata
      (error "Measure `~a` not defined." measure))
    (destructuring-bind (&key value finally type &allow-other-keys) 
	(rest metadata)
      `(let ((,ginitial (,value))
	     (,gresult nil))
	 ,@(when type
		 `((declare (type ,type ,ginitial))))
	 (unwind-protect
	      (setf ,gresult (progn ,@body))
	   (setf ,var ,@(if finally
			    `((funcall (lambda (it) ,finally)
				       (- (,value) ,ginitial)))
			    `((- ,(if type `(the ,type (,value)) `(,value))
				 ,ginitial)))))
	 ,gresult))))

(defmacro with-profile-report 
    ((name style &key 
	   (log-name *benchmark-log-path* ln-supplied?)
	   (count-calls-p *count-calls-p* ccp-supplied?)
	   (timeout nil timeout-supplied?))
     &body body)
  `(with-profile-report-fn 
       ,name ,style 
       (compile-quickly (lambda () (progn ,@body)))
       ',body
       ,@(when ccp-supplied? 
	       `(:count-calls-p ,count-calls-p))
       ,@(when ln-supplied?
	       `(:log-name ,log-name))
       ,@(when (and timeout-supplied? timeout)
	       `(:timeout ,timeout))))

(defmacro while-counting-repetitions ((&optional (delay 1.0)) &body body)
  "Execute `body` repeatedly for `delay` seconds. Returns the number
of times `body` is executed per second. Warning: assumes that `body` will not
be executed more than a fixnum number of times. The `delay` defaults to
1.0."
  (let ((gevent-count (gensym "count-"))
	(gdelay (gensym "delay-"))
	(gignore (gensym "ignore-"))
	(gfn (gensym "fn-")))
    `(let ((,gfn
	    (compile
	     nil
	     (lambda () 
	       (let ((,gevent-count 0)
		     (,gdelay ,delay))
		 (declare (type fixnum ,gevent-count))
		 (handler-case
		     (lift::with-timeout (,gdelay)
		       (loop
			  (progn ,@body)
			  (setf ,gevent-count (the fixnum (1+ ,gevent-count)))))
		   (lift::timeout-error (,gignore)
		     (declare (ignore ,gignore))
		     (if (plusp ,gevent-count)
			 (float (/ ,gevent-count ,gdelay))
			 ,gevent-count))))))))
	   (funcall ,gfn))))
  
(defmacro while-counting-events ((&optional (delay 1.0)) &body body)
  "Returns the count of the number of times `did-event` was called during 
`delay` seconds. See also: [while-counting-repetitions][]."
  (let ((gevent-count (gensym "count")))
    `(let ((,gevent-count 0))
       (flet ((did-event ()
		(incf ,gevent-count)))
	 (declare (type fixnum ,gevent-count)
		  (ignorable (function did-event)))
	 (handler-case
	     (with-timeout (,delay) 
	       (loop  
		  (progn ,@body)))
	   (timeout-error (c)
	     (declare (ignore c))
	     (float (/ ,gevent-count ,delay))))))))  

;; stolen from metatilities
(defmacro muffle-redefinition-warnings (&body body)
  "Evaluate the body so that redefinition warnings will not be 
signaled. (suppored in Allegro, Clozure CL, CLisp, and Lispworks)"
  #+allegro
  `(excl:without-redefinition-warnings
     ,@body)
  #+(or ccl mcl)
  `(let ((ccl::*warn-if-redefine* nil)
	 ;;?? FIXME not sure if this should be here or not...
	 (ccl::*record-source-file* nil))
     ,@body)
  #+clisp
  `(let ((custom:*suppress-check-redefinition* t))
    ,@body)
  #+lispworks
  `(let ((lw:*handle-warn-on-redefinition* :quiet))
    ,@body)
  #+sbcl
  ;; from http://www.sbcl.info/manual/Controlling-Verbosity.html
  `(locally (declare (sb-ext:muffle-conditions sb-ext:compiler-note
					       sb-ext::style-warning))
    ,@body)
  #-(or allegro ccl clisp mcl sbcl)
  `(progn ,@body))


(defmacro defconfig-variable (name var &optional docstring)
  (declare (ignore docstring))
  `(defmethod handle-config-preference ((name (eql ,name)) args)
     (setf ,var (first args))))

(defmacro defconfig (name &body body)
  (let ((docstring nil))
    (declare (ignorable docstring))
    (when (stringp (first body))
      (setf docstring (first body)
	    body (rest body)))
    `(defmethod handle-config-preference ((name (eql ,name)) args)
       (declare (ignorable args))
       ,@body)))

;;;;

(defmacro ensure (predicate &key report arguments)
  "If ensure's `predicate` evaluates to false, then it will generate a 
test failure. You can use the `report` and `arguments` keyword parameters
to customize the report generated in test results. For example:

    (ensure (= 23 12) 
     :report \"I hope ~a does not = ~a\" 
     :arguments (12 23))

will generate a message like

    Warning: Ensure failed: (= 23 12) (I hope 12 does not = 23)
"
  (let ((gpredicate (gensym)))
    `(let ((,gpredicate ,predicate))
       (if ,gpredicate
	   (values ,gpredicate)
	   (let ((condition (make-condition 
			     'ensure-failed-error 
			     :assertion ',predicate
			     ,@(when report
				     `(:message 
				       (format nil ,report ,@arguments))))))
	     (if (find-restart 'ensure-failed)
		 (invoke-restart 'ensure-failed condition) 
		 (warn condition)))))))

(defmacro ensure-null (predicate &key report arguments)
  "If ensure-null's `predicate` evaluates to true, then it will generate a 
test failure. You can use the `report` and `arguments` keyword parameters
to customize the report generated in test results. See [ensure][] for more 
details."
  (let ((g (gensym)))
    `(let ((,g ,predicate))
       (if (null ,g)
	   t
	 (let ((condition (make-condition 'ensure-null-failed-error
			    :value ,g
			    :assertion ',predicate
			    ,@(when report
				`(:message (format nil ,report ,@arguments))))))
	   (if (find-restart 'ensure-failed)
	       (invoke-restart 'ensure-failed condition) 
	     (warn condition)))))))

(defmacro ensure-condition (condition &body body)
  "Signal a test-failure if `body` does not signal `condition`.

If `condition` is an atom, then non-error conditions will _not_
cause a failure.

`condition` may also be a list of the form 

    (condition &key catch-all-conditions? report arguments)

If this form is used and `catch-all-conditions? is true, then 
the signaling of _any_ other condition will cause a test failure.
"
  (setf condition (remove-leading-quote condition))
  (destructuring-bind (condition &key report arguments catch-all-conditions?)
                      (if (consp condition) condition (list condition))
    (let ((g (gensym)))
      `(let ((,g nil))
         (unwind-protect
           (handler-case 
             (progn ,@body)
             (,condition (cond) 
                         (declare (ignore cond)) (setf ,g t))
             (,(if catch-all-conditions?
		   'condition 'error)
		 (cond) 
                        (setf ,g t)
                        (let ((c (make-condition 
                                  'ensure-expected-condition
                                  :expected-condition-type ',condition
                                  :the-condition cond
                                  ,@(when report
                                      `(:message 
					(format nil ,report ,arguments))))))
                          (if (find-restart 'ensure-failed)
                            (invoke-restart 'ensure-failed c) 
                            (warn c)))))
           (when (not ,g)
             (if (find-restart 'ensure-failed)
               (invoke-restart
		'ensure-failed 
		(make-condition 
		 'ensure-expected-condition
		 :expected-condition-type ',condition
		 :the-condition nil
		 ,@(when report
			 `(:message (format nil ,report ,arguments))))) 
               (warn "Ensure-condition didn't get the condition it expected."))))))))

(defmacro ensure-no-warning (&body body)
  "This macro is used to make sure that body produces no warning."
  (let ((g (gensym))
	(gcondition (gensym)))
    `(let ((,g nil)
	   (,gcondition nil))
       (unwind-protect
	    (handler-case 
		(progn ,@body)
	      (warning (c)
		(setf ,gcondition c ,g t)))
	 (when ,g
	   (let ((c (make-condition 
		    'ensure-expected-no-warning-condition
		    :the-condition ,gcondition)))
	    (if (find-restart 'ensure-failed)
		(invoke-restart 'ensure-failed c) 
		(warn c))))))))

(defmacro ensure-warning (&body body)
  "Ensure-warning evaluates its body. If the body does *not* signal a 
warning, then ensure-warning will generate a test failure."
  `(ensure-condition warning ,@body))

(defmacro ensure-error (&body body)
  "Ensure-error evaluates its body. If the body does *not* signal an 
error, then ensure-error will generate a test failure."
  `(ensure-condition error ,@body))

(defmacro ensure-same
    (form values &key (test nil test-specified-p) 
     (report nil) (arguments nil)
     (ignore-multiple-values? nil))
  "Ensure same compares value-or-values-1 value-or-values-2 or 
each value of value-or-values-1 value-or-values-2 (if they are 
multiple values) using test. If a comparison fails  
ensure-same raises a warning which uses `report` as a format string
and `arguments` as arguments to that string (if report and arguments 
are supplied). If ensure-same is used within a test, a test failure 
is generated instead of a warning"
  (%build-ensure-comparison form values 'unless 
			    test test-specified-p report arguments
			    ignore-multiple-values?))

(defmacro ensure-different
    (form values &key (test nil test-specified-p) 
     (report nil) (arguments nil)
     (ignore-multiple-values? nil))
  "Ensure-different compares value-or-values-1 value-or-values-2 or each value of value-or-values-1 and value-or-values-2 (if they are multiple values) using test. If any comparison returns true, then ensure-different raises a warning which uses report as a format string and `arguments` as arguments to that string (if report and `arguments` are supplied). If ensure-different is used within a test, a test failure is generated instead of a warning"
  (%build-ensure-comparison form values 'when
			    test test-specified-p report arguments
			    ignore-multiple-values?))

(defmacro ensure-cases ((&rest vars) (&rest cases) &body body)
  (let ((case (gensym))
	(total (gensym))
	(problems (gensym))
	(errors (gensym))
	(single-var-p (= (length vars) 1)))
    `(let ((,problems nil) (,errors nil) (,total 0))
       (loop for ,case in ,cases do
	    (incf ,total)
	    (tagbody 
	       (destructuring-bind ,vars ,(if single-var-p `(list ,case) case)
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
				       (push (list ,case condition) ,errors)
				       (go :continue))))
		       (progn ,@body))
		   (ensure-failed (cond)
		     (push (list ,case cond) ,problems))))
	       :continue))
       (if (or ,problems ,errors)
	 (let ((condition (make-condition 
			   'ensure-cases-failure
			   :total ,total
			   :problems ,problems
			   :errors ,errors)))
	   (if (find-restart 'ensure-failed)
	       (invoke-restart 'ensure-failed condition) 
	       (warn condition)))
	 ;; return true if we're happy
	 t))))

#+(or)
(defmacro ensure-member
    (form values &key (test nil test-specified-p)  
     (report nil) (arguments nil))
  "`ensure-member` checks to see if `form` is a member of `values`.

`test` is used as an argument to `member`. If `form` is not a
`member`, then ensure-member raises a warning which uses `report` as a
format string and `arguments` as arguments to that string (if report
and arguments are supplied). If `ensure-member` is used within a test, a
test failure is generated instead of a warning"
)

(defmacro with-test-slots (&body body)
  `(symbol-macrolet ((lift-result (getf (test-data *current-test*) :result)))   
     ;; case111 - LW complains otherwise
     (declare (ignorable lift-result)
	      ,@(when (def :dynamic-variables)
		      `((special ,@(mapcar #'car (def :dynamic-variables))))))
     (symbol-macrolet
	 ,(mapcar #'(lambda (local)
		      `(,local (test-environment-value ',local)))
		  (test-slots (def :testsuite-name)))
       (declare (ignorable ,@(test-slots (def :testsuite-name))))
       (macrolet
	   ,(mapcar (lambda (spec)
		      (destructuring-bind (name arglist) spec
			`(,name ,arglist 
				`(flet-test-function 
				  *current-test* ',',name ,,@arglist))))
		    (def :function-specs))
	 (progn ,@body)))))

;;;;

(defmacro append-to-report ((var output-to) &body body)
  (let ((gclosep (gensym "closep"))
	(gstream (gensym "stream")))
    `(let* ((,gclosep nil)
	    (,gstream ,output-to)
	    (,var (etypecase ,gstream 
		    (stream ,gstream)
		    ((or pathname string)
		     (setf ,gclosep t)
		     (open ,gstream 
			   :if-does-not-exist :create
			   :if-exists :append
			   :direction :output)))))
       (unwind-protect
	    (labels ((out (key value)
		       (when value
			 (let ((*print-readably* nil))
			   (format out "~&\(~s . ~s\)" key value)))))
	      (declare (ignorable (function out)))
	      (progn ,@body)
	      (force-output out))
	 (when ,gclosep
	   (close ,var))))))
