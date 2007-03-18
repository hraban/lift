(in-package #:lift)

(defun run-tests-from-file (path)
  (let ((*package* *package*)
	(*read-eval* nil)
	(form nil)
	(result (make-test-result path :multiple)))
    (with-open-file (in path
			:direction :input
			:if-does-not-exist :error)
      (let ((*lift-dribble-pathname* nil)
	    (*lift-debug-io* *debug-io*)
	    (*lift-standard-output* *standard-output*)
	    (*test-break-on-errors?* nil)
	    (*test-do-children?* t)
	    (*lift-equality-test* 'equal)
	    (*test-print-length* :follow-print)
	    (*test-print-level* :follow-print)
	    (*lift-if-dribble-exists* :append))
	(loop while (not (eq (setf form (read in nil :eof nil)) :eof)) collect
	     (destructuring-bind (name &rest args &key &allow-other-keys)
		 form
	       (assert (typep name 'symbol))
	       (cond 
		 ;; check for preferences first (i.e., keywords)
		 ((eq (symbol-package name) 
		      (symbol-package :keyword))
		  ;; must be a preference
		  (handle-config-preference name args))
		 ((subtypep (find-test-suite name)
			    'lift:test-mixin)
		  (apply #'run-tests :suite name 
			 :result result (massage-arguments args)))
		 (t
		  (error "Don't understand '~s' while reading from ~s" 
			 form path)))))))
    (values result)))

(defun massage-arguments (args)
  (loop for arg in args collect
       (cond ((eq arg '*standard-output*) *standard-output*)
	     (t arg))))

(defmethod handle-config-preference ((name t) args)
  (declare (ignore args))
  (error "Unknown preference ~s (with arguments ~s)" 
	 name args))

(defmethod handle-config-preference ((name (eql :dribble)) args)
  (declare (ignore args))
  (setf *lift-dribble-pathname* (first args)))

(defmethod handle-config-preference ((name (eql :debug-io)) args)
  (declare (ignore args))
  (setf *lift-debug-io* (first args)))

(defmethod handle-config-preference ((name (eql :standard-output)) args)
  (declare (ignore args))
  (setf *lift-standard-output* (first args)))

(defmethod handle-config-preference ((name (eql :break-on-errors?)) args)
  (declare (ignore args))
  (setf *test-break-on-errors?* (first args)))

(defmethod handle-config-preference ((name (eql :do-children?)) args)
  (declare (ignore args))
  (setf *test-do-children?* (first args)))

(defmethod handle-config-preference ((name (eql :equality-test)) args)
  (declare (ignore args))
  (setf *lift-equality-test* (first args)))

(defmethod handle-config-preference ((name (eql :print-length)) args)
  (declare (ignore args))
  (setf *test-print-length* (first args)))

(defmethod handle-config-preference ((name (eql :print-level)) args)
  (declare (ignore args))
  (setf *test-print-level* (first args)))

(defmethod handle-config-preference ((name (eql :if-dribble-exists))
				     args)
  (declare (ignore args))
  (setf *lift-if-dribble-exists* (first args)))

