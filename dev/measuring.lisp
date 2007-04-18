(in-package #:lift)

(declaim (optimize (speed 3) (safety 1)))

(defmacro with-measuring ((var measure-fn) &body body)
  (let ((initial (gensym)))
    `(let ((,initial (,measure-fn)))
       ,@body
       (setf ,var (- (,measure-fn) ,initial)))))

(defmacro measure-time ((var) &body body)
  `(prog1
       (with-measuring (,var get-internal-real-time)
	 ,@body)
     (setf ,var (coerce (/ ,var internal-time-units-per-second) 
			'double-float))))

(defmacro measure-conses ((var) &body body)
  `(with-measuring (,var total-bytes-allocated)
     ,@body))

(defun measure-fn (fn &rest args)
  (declare (dynamic-extent args))
  (let ((bytes 0) (seconds 0) result)
    (measure-time (seconds)
      (measure-conses (bytes)
	(setf result (apply fn args))))
    (values seconds bytes result)))

(defmacro measure (seconds bytes &body body)
  (let ((result (gensym)))
    `(let (,result)
       (measure-time (,seconds)
	 (measure-conses (,bytes)
	   (setf ,result ,@body)))
       (values ,result))))

(defparameter *benchmark-file*
  (asdf:system-relative-pathname 
   'lift "benchmark-data/benchmarks.log"))

(defvar *additional-markers* nil)

#+allegro
(defun cancel-current-profile (&key force?)
  (unless force?
    (assert (member (prof:profiler-status) '(:inactive))))
  (prof:stop-profiler)
  (setf prof::*current-profile* (prof::make-current-profile)))

#+allegro
(defun current-profile-sample-count ()
  (ecase (prof::profiler-status)
    ((:inactive :analyzed) 0)
    ((:suspended :saved)
     (slot-value (prof::current-profile-actual prof::*current-profile*) 
		 'prof::samples))
    (:sampling (warn "Can't determine count while sampling"))))

#+allegro
(defmacro with-profile-report ((name style &key (log-name *benchmark-file*)) 
			       &body body)
  (assert (member style '(:time :space)))
  `(let ((seconds 0.0) (conses 0) result)
     (cancel-current-profile)
     (multiple-value-prog1
	 (prof:with-profiling (:type ,style)
	   (measure seconds conses ,@body))
       (let ((date-stamp (get-universal-time)))
	 (ensure-directories-exist ,log-name)
	 ;;log 
	 (with-open-file (output ,log-name
				 :direction :output
				 :if-does-not-exist :create
				 :if-exists :append)
	   (with-standard-io-syntax
	     (let ((*print-readably* nil))
	       (terpri output)
	       (format output "\(~11,d ~20,s ~10,s ~10,s ~{~s~^ ~} ~s\)"
		       date-stamp ,name 
		       seconds conses *additional-markers*
		       result))))
	 (when (> (current-profile-sample-count) 0)
	   (let ((pathname (merge-pathnames
			    (make-pathname 
			     :type "prof"
			     :name (format nil "~a-~a-~a"
					   ,name ,style date-stamp))
			    ,log-name)))
	     (format t "~&Profiling output being sent to ~a" pathname)
	     (with-open-file (output pathname
				     :direction :output
				     :if-does-not-exist :create
				     :if-exists :append)
	       (format output "~&Profile data for ~a" ,name)
	       (format output "~&Date: ~a" 
		       (excl:locale-print-time (get-universal-time)
					       :fmt "%B %d, %Y %T" :stream nil))
	       (format output "~&  Total time: ~,2F; Total space: ~:d \(~:*~d\)"
		       seconds conses)
	       (format output "~%~%")
	       (when (or (eq :time ,style)
			 (eq :space ,style))
		 (prof:show-flat-profile :stream output)
		 (prof:show-call-graph :stream output)))))))))

#| OLD
;; integrate with LIFT

(pushnew :measure *deftest-clauses*)

(add-code-block
 :measure 1 :class-def
 (lambda () (def :measure)) 
 '((setf (def :measure) (cleanup-parsed-parameter value)))
 (lambda ()
   (pushnew 'measured-test-mixin (def :superclasses))
   nil))

(defclass measured-test-mixin ()
  ((total-conses :initform 0
		 :accessor total-conses)
   (total-seconds :initform 0
		  :accessor total-seconds)))
|#